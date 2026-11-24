namespace Hearts.Experiments

open System
open System.IO

open TorchSharp
open type TorchSharp.torch.nn

/// Fits W, each seat's chance of winning the game from the start of a deal, to the data from WData. From
/// one seat's point of view, the inputs are the four game scores, the seat's own first and then the others'
/// in playing order, each divided by 100; the output is a softmax over the same four seats. Each deal is
/// counted from all four seats' points of view. One game in ten is held out for validation. (The deal's
/// pass direction, which WData records, improved the validation loss by only 0.0001 on 100,000 games, and
/// made W noisier, so W leaves it out.)
module WFit =

    /// One deal at its start, from WData.
    type private Row = { Scores : int[]; Winner : int; Game : int }

    let private readRows (path : string) =
        File.ReadLines(path)
            |> Seq.skip 1
            |> Seq.map (fun line ->
                let xs = line.Split(',') |> Array.map int
                xs[0 .. 3], xs[5])
            |> Seq.mapFold (fun game (scores, winner) ->
                let game = if Array.forall ((=) 0) scores then game + 1 else game   // each game starts at zero
                { Scores = scores; Winner = winner; Game = game }, game) -1
            |> fst
            |> Seq.toArray

    /// The inputs for the given scores (absolute seats), from the given seat's point of view.
    let private features (scores : int[]) seat =
        [| for k in 0 .. 3 -> float32 scores[(seat + k) % 4] / 100.0f |]

    /// The network.
    let create () =
        Sequential(
            Linear(4L, 64L), ReLU(),
            Linear(64L, 64L), ReLU(),
            Linear(64L, 4L))

    /// Every row from every seat's point of view: inputs, and the winner relative to that seat.
    let private samples (rows : Row[]) =
        let xs, ys =
            [| for row in rows do
                for seat in 0 .. 3 ->
                    features row.Scores seat, int64 ((row.Winner - seat + 4) % 4) |]
                |> Array.unzip
        torch.tensor(array2D xs), torch.tensor(ys)

    /// Mean cross-entropy of the network on the given samples.
    let private loss (net : Module<torch.Tensor, torch.Tensor>) (x : torch.Tensor) (y : torch.Tensor) =
        use _ = torch.no_grad()
        use logits = net.forward(x)
        use l = functional.cross_entropy(logits, y)
        l.item<float32>()

    /// Each seat's chance of winning, from the given seat's point of view (the given seat first).
    let predict (net : Module<torch.Tensor, torch.Tensor>) (scores : int[]) seat =
        use _ = torch.no_grad()
        use x = torch.tensor(array2D [| features scores seat |])
        use p = functional.softmax(net.forward(x), 1L)
        p.data<float32>().ToArray()

    /// Trains a network, answering it and its best validation loss.
    let private train (trainRows : Row[]) (validRows : Row[]) numEpochs =
        torch.manual_seed(0L) |> ignore
        let net = create ()
        let xTrain, yTrain = samples trainRows
        let xValid, yValid = samples validRows
        use optimizer = torch.optim.Adam(net.parameters(), 1e-3)
        let n = xTrain.shape[0]
        let batchSize = 2048L
        let mutable best = Single.MaxValue
        let mutable bestState = None
        for epoch in 1 .. numEpochs do
            net.train()
            use perm = torch.randperm(n)
            for start in 0L .. batchSize .. n - 1L do
                use idx = perm.narrow(0L, start, min batchSize (n - start))
                use x = xTrain.index_select(0L, idx)
                use y = yTrain.index_select(0L, idx)
                optimizer.zero_grad()
                use logits = net.forward(x)
                use l = functional.cross_entropy(logits, y)
                l.backward()
                optimizer.step() |> ignore
            net.eval()
            let v = loss net xValid yValid
            if v < best then
                best <- v
                bestState <- Some (net.state_dict() |> Seq.map (fun kv -> kv.Key, kv.Value.clone()) |> dict)
            if epoch % 5 = 0 || epoch = 1 then
                printfn $"   epoch {epoch}: train %.5f{loss net xTrain yTrain}, validation %.5f{v}"
        net.load_state_dict(Collections.Generic.Dictionary(bestState.Value)) |> ignore
        net.eval()
        net, best

    /// Calibration on the validation games: the chance the model gives a seat, by tenths, against how often
    /// that seat won.
    let private calibrate (net : Module<torch.Tensor, torch.Tensor>) (rows : Row[]) =
        let buckets = Array.init 10 (fun _ -> ResizeArray<float32 * bool>())
        for row in rows do
            for seat in 0 .. 3 do
                let p = (predict net row.Scores seat)[0]
                buckets[min 9 (int (p * 10.0f))].Add(p, row.Winner = seat)
        printfn "   calibration (predicted vs actual win rate, validation):"
        for i, b in Seq.indexed buckets do
            if b.Count > 0 then
                let predicted = b |> Seq.averageBy (fst >> float)
                let actual = b |> Seq.averageBy (fun (_, won) -> if won then 1.0 else 0.0)
                printfn $"     %.1f{float i / 10.0}-%.1f{float (i + 1) / 10.0}: predicted %.3f{predicted}, actual %.3f{actual}  ({b.Count})"

    /// Fits W, reports it, and saves it.
    let run (dataPath : string) (outPath : string) numEpochs =
        let rows = readRows dataPath
        let numGames = (rows |> Array.last).Game + 1
        let validRows, trainRows = rows |> Array.partition (fun r -> r.Game % 10 = 0)
        printfn $"{numGames} games, {rows.Length} deals: {trainRows.Length} for training, {validRows.Length} for validation (x4 seats)"
        printfn $"uniform guess: %.5f{log 4.0}"
        let net, best = train trainRows validRows numEpochs
        printfn $"   best validation loss %.5f{best}"
        calibrate net validRows

        let show scores =
            let label = String.Join(",", scores |> Array.map string)
            let p = String.Join(" ", predict net scores 0 |> Array.map (fun x -> x.ToString("0.000")))
            printfn $"   %-16s{label}: {p}"
        printfn "   W from seat 0's point of view (scores in playing order from seat 0):"
        show [| 0; 0; 0; 0 |]
        show [| 13; 0; 0; 0 |]
        show [| 50; 50; 50; 50 |]
        show [| 63; 50; 50; 50 |]
        show [| 80; 70; 70; 70 |]
        show [| 93; 70; 70; 70 |]
        show [| 60; 90; 75; 80 |]
        net.save(outPath) |> ignore
        printfn $"saved W to {outPath}"
