namespace Hearts.DeepCfr

open System.Diagnostics
open MathNet.Numerics.LinearAlgebra
open TorchSharp
open PlayingCards

open Hearts

module Trainer =

    /// Assume two-player, zero-sum game.
    let private numPlayers = 2

    let private tryGetPayoff deal =
        Game.tryUpdateScore deal Score.zero
            |> Option.map (fun score ->
                let otherAvg =
                    (score.ScoreMap
                        |> Map.toSeq
                        |> Seq.where (fun (seat, _) -> seat <> Seat.South)
                        |> Seq.sumBy snd
                        |> float)
                        / float (Seat.numSeats - 1)
                otherAvg - float score[Seat.South])

    /// Computes strategy for the given info set using the
    /// given advantage model.
    let getStrategy infoSetKey model indexes =
        use _ = torch.no_grad()   // use model.eval() instead?
        let wide =
            (AdvantageModel.getAdvantage infoSetKey model)
                .data<float32>()
                |> DenseVector.ofSeq
        indexes
            |> Seq.map (fun idx -> wide[idx])
            |> DenseVector.ofSeq
            |> InformationSet.getStrategy

    /// Negates opponent's utilties (assuming a zero-zum game).
    let private getActiveUtilities utilities =
        utilities
            |> Seq.map (~-)
            |> DenseVector.ofSeq

    /// Evaluates the utility of the given deal.
    let private traverse iter deal updatingPlayer (models : _[]) =

        /// Appends an item to the end of an array.
        let append items item =
            [| yield! items; yield item |]

        /// Top-level loop.
        let rec loop deal =
            match tryGetPayoff deal with
                | Some payoff ->
                    float32 payoff, Array.empty   // game is over
                | None ->
                    loopNonTerminal deal

        /// Recurses for non-terminal game state.
        and loopNonTerminal (deal : OpenDeal) =

                // get info set for current state from this player's point of view
            let activePlayer =
                if OpenDeal.currentPlayer deal = Seat.South then 0
                else 1
            let hand = OpenDeal.currentHand deal
            let infoSetKey =
                {
                    Hand = hand
                    Deal = deal.ClosedDeal
                }

                // get active player's current strategy for this info set
            let legalPlays =
                deal.ClosedDeal
                    |> ClosedDeal.legalPlays hand
                    |> Seq.toArray
            let strategy =
                legalPlays
                    |> Seq.map Card.toIndex
                    |> getStrategy infoSetKey models[activePlayer]

                // get utility of this info set
            if activePlayer = updatingPlayer then

                    // get utility of each action
                let actionUtilities, samples =
                    let utilities, sampleArrays =
                        legalPlays
                            |> Array.map (fun play ->
                                deal
                                    |> OpenDeal.addPlay play
                                    |> loop)
                            |> Array.unzip
                    getActiveUtilities utilities,
                    Array.concat sampleArrays

                    // utility of this info set is action utilities weighted by action probabilities
                let utility = actionUtilities * strategy
                let sample =
                    let wideRegrets =
                        let narrowRegrets = actionUtilities - utility
                        assert(narrowRegrets.Count = legalPlays.Length)
                        Seq.zip legalPlays narrowRegrets
                            |> Encoding.encodeCardValues
                            |> DenseVector.ofArray
                    AdvantageSample.create
                        infoSetKey
                        wideRegrets
                        iter |> Choice1Of2
                utility, append samples sample

            else
                    // sample a single action according to the strategy
                let utility, samples =
                    let play =
                        strategy
                            |> Vector.sample settings.Random
                            |> Array.get legalPlays
                    deal
                       |> OpenDeal.addPlay play
                       |> loop
                let sample =
                    let wideStrategy =
                        assert(strategy.Count = legalPlays.Length)
                        Seq.zip legalPlays strategy
                            |> Encoding.encodeCardValues
                            |> DenseVector.ofArray
                    StrategySample.create
                        infoSetKey
                        wideStrategy
                        iter |> Choice2Of2
                -utility, append samples sample

        loop deal |> snd

    /// Advantage state managed for each player.
    type private AdvantageState =
        {
            /// Player's model.
            Model : AdvantageModel

            /// Player's reservoir.
            Reservoir : Reservoir<AdvantageSample>
        }

    module private AdvantageState =

        /// Creates an advantage model.
        let private createModel () =
            AdvantageModel.create
                settings.HiddenSize
                settings.LearningRate

        /// Creates an advantage state.
        let create () =
            {
                Model = createModel ()
                Reservoir =
                    Reservoir.create
                        settings.Random
                        settings.NumAdvantageSamples
            }

        /// Resets the model of the given state.
        let resetModel state =
            {
                state with
                    Model = createModel ()
            }

    /// Generates training data for the given player.
    let private generateSamples iter updatingPlayer stateMap =

        let nDeals =
            let factor =
                if updatingPlayer = 0 then
                    settings.CutthroatCompensation
                else 1
            factor * settings.NumTraversals

        Choice.unzip [|
            for iDeal = 0 to nDeals do
                let deal =
                    let deck = Deck.shuffle settings.Random
                    let dealer = enum<Seat> (iDeal % Seat.numSeats)
                    OpenDeal.fromDeck
                        dealer
                        ExchangeDirection.Hold
                        deck
                        |> OpenDeal.startPlay
                let models =
                    stateMap
                        |> Map.values
                        |> Seq.map (fun state -> state.Model)
                        |> Seq.toArray
                yield! traverse
                    iter deal updatingPlayer models
        |]

    /// Adds the given samples to the given reservoir and then
    /// uses the reservoir to train the given advantage model.
    let private trainAdvantageModel state newSamples =

        let resv =
            Reservoir.addMany newSamples state.Reservoir

        let stopwatch = Stopwatch.StartNew()
        let losses =
            AdvantageModel.train resv.Items state.Model
        if settings.Verbose then
            stopwatch.Stop()
            printfn $"   Trained model on {resv.Items.Count} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float resv.Items.Count} ms/sample)"

        resv, losses

    /// Trains a player.
    let private trainPlayer iter stateMap updatingPlayer =

        if settings.Verbose then
            printfn $"\nTraining player {updatingPlayer}"

            // generate training data for this player
        let advSamples, stratSamples =
            generateSamples iter updatingPlayer stateMap
        if settings.Verbose then
            printfn $"   {advSamples.Length} advantage samples generated"
            printfn $"   {stratSamples.Length} strategy samples generated"

            // train this player's model
        let state =
            stateMap[updatingPlayer]
                |> AdvantageState.resetModel
        let resv, losses =
            trainAdvantageModel state advSamples
        let stateMap =
            let state = { state with Reservoir = resv }
            Map.add updatingPlayer state stateMap

            // log inputs and losses
        settings.Writer.add_scalar(
            $"advantage reservoir/player{updatingPlayer}",
            float32 resv.Items.Count,
            iter)
        for epoch = 0 to losses.Length - 1 do
            settings.Writer.add_scalar(
                $"advantage loss/iter%04d{iter}/player{updatingPlayer}",
                losses[epoch], epoch)

        stratSamples, stateMap

    let private createChallenger model =

        let play model hand deal =
            let infoSetKey =
                {
                    Hand = hand
                    Deal = deal
                }
            let legalPlays =
                deal
                    |> ClosedDeal.legalPlays hand
                    |> Seq.toArray
            let strategy =
                legalPlays
                    |> Seq.map Card.toIndex
                    |> getStrategy infoSetKey model
            strategy
                |> Vector.sample settings.Random
                |> Array.get legalPlays

        { Play = play model }

    let private runTournament challenger =
        let playerMap =
            Enum.getValues<Seat>
                |> Seq.map (fun seat ->
                    let player =
                        if seat = Seat.South then challenger
                        else Champion.player
                    seat, player)
                |> Map
        let score =
            Game.playDeals
                settings.Random
                settings.NumEvaluationDeals
                playerMap
        printfn "\nTournament score:"
        for (KeyValue(seat, points)) in score.ScoreMap do
            printfn $"   {seat}: {points}"

    /// Trains a single iteration.
    let private trainIteration iter stateMap =

        if settings.Verbose then
            printfn $"\n*** Iteration {iter} ***"

            // train each player's model
        let stratSampleSeqs, stateMap =
            Seq.mapFold
                (trainPlayer iter)
                stateMap
                (seq { 0 .. numPlayers - 1 })

        let challenger = createChallenger stateMap[0].Model
        runTournament challenger

        stateMap, Seq.concat stratSampleSeqs

    /// Trains a strategy model using the given samples.
    let private trainStrategyModel (resv : Reservoir<_>) =

        if settings.Verbose then
            printfn $"\n*** Training strategy model ***"

        let model =
            StrategyModel.create
                settings.HiddenSize
                settings.LearningRate
        let losses =
            StrategyModel.train resv.Items model
        if settings.Verbose then
            printfn $"Trained model on {resv.Items.Count} samples"

        for epoch = 0 to losses.Length - 1 do
            settings.Writer.add_scalar(
                "strategy loss", losses[epoch], epoch)

        model

    /// Trains for the given number of iterations.
    let train () =

        if settings.Verbose then
            printfn $"{settings}"

            // create advantage state
        let advStateMap =
            Map [|
                for player = 0 to numPlayers - 1 do
                    player, AdvantageState.create ()
            |]

            // run the iterations
        let _, stratResv =
            let stratResv =
                Reservoir.create
                    settings.Random
                    settings.NumStrategySamples
            let iterNums = seq { 0 .. settings.NumIterations - 1 }
            ((advStateMap, stratResv), iterNums)
                ||> Seq.fold (fun (advStateMap, stratResv) iter ->
                    let advStateMap, stratSamples =
                        trainIteration iter advStateMap
                    let stratResv =
                        Reservoir.addMany stratSamples stratResv
                    settings.Writer.add_scalar(
                        $"strategy reservoir",
                        float32 stratResv.Items.Count,
                        iter)
                    advStateMap, stratResv)

            // train the final strategy model
        trainStrategyModel stratResv
