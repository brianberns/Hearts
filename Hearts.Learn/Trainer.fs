namespace Hearts.Learn

open System
open System.Diagnostics
open System.IO

open MathNet.Numerics.LinearAlgebra

open PlayingCards
open Hearts

module Trainer =

    /// Generates training data using the given model.
    let private generateSamples model =

        let collect =
#if DEBUG
            Array.collect
#else
            Array.Parallel.collect
#endif

        Array.init settings.NumTraversals id
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
                Traverse.traverse deal model)

    /// Trains the given advantage model using the given samples.
    let private trainAdvantageModel samples model =

        let stopwatch = Stopwatch.StartNew()
        let losses = AdvantageModel.train samples model
        if settings.Verbose then
            stopwatch.Stop()
            printfn $"   Trained model on {samples.Length} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float samples.Length} ms/sample)"
        losses

    /// Trains a new model using the given model.
    let private updateModel iter model =

            // generate training data from existing model
        let stopwatch = Stopwatch.StartNew()
        let samples = generateSamples model
        if settings.Verbose then
            printfn $"   {samples.Length} samples generated in {stopwatch.Elapsed}"

            // train a new model
        model.Dispose()
        let model = new AdvantageModel()
        let losses = trainAdvantageModel samples model
        Path.Combine(
            settings.ModelDirPath,
            $"AdvantageModel%03d{iter}.pt")
                |> model.save
                |> ignore

            // log inputs and losses
        for epoch = 0 to losses.Length - 1 do
            settings.Writer.add_scalar(
                $"advantage loss/iter%03d{iter}",
                losses[epoch], epoch)

        model

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
    let private trainIteration iter model =

        if settings.Verbose then
            printfn $"\n*** Iteration {iter} ***"

            // train a new model
        let model = updateModel iter model

            // evaluate model
        let avgPayoff =
            let challenger =
                createChallenger (
                    Strategy.getFromAdvantage model)
            Tournament.run
                (Random(0))   // use same deals each iteration
                Database.player
                challenger
        settings.Writer.add_scalar(
            $"advantage tournament", avgPayoff, iter)

        model

    /// Trains for the given number of iterations.
    let train () =

        if settings.Verbose then
            printfn $"Server garbage collection: {System.Runtime.GCSettings.IsServerGC}"
            printfn $"Settings: {settings}"

            // create initial model
        let model = new AdvantageModel()
        let nParms =
            model.parameters(true)
                |> Seq.where (fun parm -> parm.requires_grad)
                |> Seq.sumBy (fun parm -> parm.numel())
        settings.Writer.add_text(
            $"settings/AdvModelParmCount", string nParms, 0)
        if settings.Verbose then
            printfn $"Model input size: {Network.inputSize}"
            printfn $"Model output size: {Network.outputSize}"
            printfn $"Advantage model parameter count: {nParms}"

            // run the iterations
        let model =
            let iterNums = seq { 0 .. settings.NumIterations - 1 }
            (model, iterNums)
                ||> Seq.fold (fun model iter ->
                    trainIteration iter model)
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
                            hand deal.ClosedDeal regrets
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
