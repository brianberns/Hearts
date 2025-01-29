namespace Hearts.DeepCfr

open System
open System.Diagnostics
open System.IO

open MathNet.Numerics.LinearAlgebra

open PlayingCards
open Hearts

module Trainer =

    /// Advantage state managed for each player.
    type private AdvantageState =
        {
            /// Player's model.
            Model : AdvantageModel

            /// Player's reservoir.
            Reservoir : Reservoir<AdvantageSample>
        }

    module private AdvantageState =

        /// Creates an advantage state.
        let create () =
            {
                Model = new AdvantageModel()
                Reservoir =
                    Reservoir.create
                        settings.Random
                        settings.NumAdvantageSamples
            }

        /// Resets the model of the given state.
        let resetModel state =
            state.Model.Dispose()
            {
                state with
                    Model = new AdvantageModel()
            }

    /// Generates training data for the given player.
    let private generateSamples iter updatingPlayer stateMap =

        let nDeals =
            let factor =
                if updatingPlayer = 0 then
                    settings.ZeroSumCompensation
                else 1
            factor * settings.NumTraversals

        let collect =
#if DEBUG
            Array.collect
#else
            Array.Parallel.collect
#endif

        Array.init nDeals id
            |> collect (fun iDeal ->
                let deal =
                    let deck =
                        lock settings.Random (fun () ->
                            Deck.shuffle settings.Random)
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
                Traverse.traverse
                    iter deal updatingPlayer models)
            |> Choice.unzip

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
        let stopwatch = Stopwatch.StartNew()
        let advSamples, stratSamples =
            generateSamples iter updatingPlayer stateMap
        if settings.Verbose then
            printfn $"   {advSamples.Length} advantage samples, {stratSamples.Length} strategy samples generated in {stopwatch.Elapsed}"

            // train this player's model
        let state =
            let state = stateMap[updatingPlayer]
            if settings.ResetAdvantageModel then
                AdvantageState.resetModel state
            else state
        let resv, losses =
            trainAdvantageModel state advSamples
        if updatingPlayer = 0 then
            Path.Combine(
                settings.ModelDirPath,
                $"AdvantageModel%03d{iter}.pt")
                    |> state.Model.save
                    |> ignore
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
                $"advantage loss/iter%03d{iter}/player{updatingPlayer}",
                losses[epoch], epoch)

        stratSamples, stateMap

    let createChallenger getStrategy =

        let play hand deal =
            let legalPlays =
                deal
                    |> ClosedDeal.legalPlays hand
                    |> Seq.toArray
            let strategy =
                getStrategy hand deal legalPlays
            strategy
                |> Vector.sample settings.Random
                |> Array.get legalPlays

        { Play = play }

    /// Trains a single iteration.
    let private trainIteration iter stateMap =

        if settings.Verbose then
            printfn $"\n*** Iteration {iter} ***"

            // train each player's model
        let stratSampleSeqs, stateMap =
            Seq.mapFold
                (trainPlayer iter)
                stateMap
                (seq { 0 .. ZeroSum.numPlayers - 1 })

            // evaluate model
        let avgPayoff =
            let challenger =
                createChallenger (
                    Strategy.getFromAdvantage stateMap[0].Model)
            Tournament.run
                (Random(0))   // use same deals each iteration
                Database.player
                challenger
        settings.Writer.add_scalar(
            $"advantage tournament", avgPayoff, iter)

        stateMap, Seq.concat stratSampleSeqs

    /// Trains a strategy model using the given samples.
    let private trainStrategyModel (resv : Reservoir<_>) =

        if settings.Verbose then
            printfn $"\n*** Training strategy model ***"

        let model = new StrategyModel()
        let losses =
            StrategyModel.train resv.Items model
        if settings.Verbose then
            printfn $"\nTrained model on {resv.Items.Count} samples"

        for epoch = 0 to losses.Length - 1 do
            settings.Writer.add_scalar(
                "strategy loss", losses[epoch], epoch)

            // evaluate model
        let avgPayoff =
            let challenger =
                createChallenger (
                    Strategy.getFromStrategy model)
            Tournament.run
                (Random(0))   // use same deals each iteration
                Database.player
                challenger
        settings.Writer.add_scalar(
            $"strategy tournament",
            avgPayoff, settings.NumIterations)

        model

    /// Trains for the given number of iterations.
    let train () =

        if settings.Verbose then
            printfn $"Server garbage collection: {System.Runtime.GCSettings.IsServerGC}"
            printfn $"Settings: {settings}"

            // create advantage state
        let advStateMap =
            Map [|
                for player = 0 to ZeroSum.numPlayers - 1 do
                    player, AdvantageState.create ()
            |]
        let nParms =
            advStateMap[0].Model.parameters(true)
                |> Seq.where (fun parm -> parm.requires_grad)
                |> Seq.sumBy (fun parm -> parm.numel())
        settings.Writer.add_text(
            $"settings/AdvModelParmCount", string nParms, 0)
        if settings.Verbose then
            printfn $"Advantage model parameter count: {nParms}"

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
        let model = trainStrategyModel stratResv
        Path.Combine(
            settings.ModelDirPath,
            "StrategyModel.pt")
                |> model.save
                |> ignore
        model

    let private createTrainingData numDeals =

        let conn = Hearts.Web.Database.connect "."

        let rec loop deal =
            seq {
                let hand =
                    let seat = OpenDeal.currentPlayer deal
                    deal.UnplayedCardMap[seat]
                let legalPlays =
                    ClosedDeal.legalPlays hand deal.ClosedDeal
                        |> Seq.toArray
                let adjustedDeal =
                    Hearts.FastCfr.ClosedDeal.adjustDeal
                        Seat.South
                        deal.ClosedDeal
                let strategyOpt =
                    if legalPlays.Length = 1 then None
                    else
                        adjustedDeal
                            |> Hearts.FastCfr.GameState.getInfoSetKey hand
                            |> Hearts.Web.Database.tryGetStrategy conn
                match strategyOpt with
                    | Some strategy ->
                        let regrets =
                            strategy
                                |> Array.map float32
                                |> DenseVector.ofArray
                                |> Strategy.toWide legalPlays
                        yield AdvantageSample.create
                            hand deal.ClosedDeal regrets 0
                    | None -> ()

                let deal =
                    let card =
                        let index =
                            match strategyOpt with
                                | Some strategy ->
                                    MathNet.Numerics.Distributions.Categorical.Sample(
                                        settings.Random,
                                        strategy)
                                | None -> 0
                        legalPlays[index]
                    OpenDeal.addPlay card deal
                match Game.tryUpdateScore deal Score.zero with
                    | Some _ -> ()
                    | None -> yield! loop deal
            }

        seq {
            for _ = 1 to numDeals do
                let deal =
                    let deck = Deck.shuffle settings.Random
                    OpenDeal.fromDeck
                        Seat.South
                        ExchangeDirection.Hold
                        deck
                        |> OpenDeal.startPlay
                yield! loop deal
        }

    let trainDirect numDeals =

        printfn $"{settings}"
        printfn $"numDeals: {numDeals}"
        let samples =
            createTrainingData numDeals
                |> Seq.toArray
        printfn $"Number of samples: {samples.Length}"

        let model = new AdvantageModel()
        let losses = AdvantageModel.train samples model
        printfn $"Final loss {Array.last losses}"

        let pairs =
            [
                "Random", Tournament.randomPlayer
                "Database", Database.player
            ]
        for name, champion in pairs do
            let avgPayoff =
                let challenger = createChallenger (
                    Strategy.getFromAdvantage model)
                Tournament.run
                    settings.Random
                    champion
                    challenger
            printfn $"\nAverage payoff vs. {name}: {avgPayoff}"
