namespace Hearts.Generate

open TorchSharp
open type torch.nn
open type torch.nn.functional
open FSharp.Core.Operators   // reclaim "float32" and other F# operators

open Hearts
open Hearts.Model

/// Each seat's chance of winning the game from the start of a
/// deal, given the game score, as fitted by Hearts.Experiments'
/// wfit mode. Proposed and originally written by Claude, with
/// my changes.
module WinProbability =

    /// Loads trained model from the given file.
    let load (path : string) : Model =
        let hiddenSize = 64
        let model =
            Sequential(
                Linear(Seat.numSeats, hiddenSize),   // input: game score
                ReLU(),
                Linear(hiddenSize, hiddenSize),
                ReLU(),
                Linear(hiddenSize, Seat.numSeats))   // output: each seat's chance of winning the game
        model.load(path) |> ignore
        model.eval()
        model

    /// The model's chances, indexed by seat. The model sees the
    /// scores from one seat's point of view (that seat first,
    /// then the others in playing order), so this cycles through
    /// all the seats and averages the answers.
    let private evaluate (model : Model) (gameScore : Score) =
        use _ = torch.no_grad()

            // cycle game score seats
        use input =
            array2D [|
                for viewer in Seat.allSeats do
                    Seat.cycle viewer
                        |> Seq.map (fun seat ->
                            float32 gameScore[seat] / 100.0f)
            |] |> torch.tensor

            // get probabilities
        use output = input --> model
        use probs = softmax(output, 1)
        let data = probs.data<float32>().ToNDArray() :?> float32[,]   // row = viewer, column = seat relative to the viewer

            // average each seat's probability
        [|
            for seat in Seat.allSeats do
                Seat.allSeats
                    |> Array.averageBy (fun viewer ->
                        data[int viewer, Seat.getIndex seat viewer])
        |]

    /// Gets each seat's chance of winning the game from the given
    /// game score.
    let getWinChances rules model gameScore =
        let winners =
            GameScore.findGameWinners rules.TieGame gameScore
        if winners.IsEmpty then
            evaluate model gameScore
        else
            let share = 1.0f / float32 winners.Count
            Seat.allSeats
                |> Array.map (fun seat ->
                    if winners.Contains(seat) then share
                    else 0.0f)

    /// Gets each seat's payoff for a deal, indexed by seat: the
    /// change in its chance of winning the game, from the game
    /// score before the deal to the game score after it (which
    /// should already include the shoot reward, if any). Each
    /// deal's payoffs sum to zero.
    let getPayoff rules model before after =
        let pBefore = getWinChances rules model before
        let pAfter = getWinChances rules model after
        Array.map2 (-) pAfter pBefore
