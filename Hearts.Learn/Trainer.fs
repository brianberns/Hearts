namespace Hearts.Learn

open System
open System.Diagnostics
open System.IO

open MathNet.Numerics.LinearAlgebra

open PlayingCards
open Hearts
open Hearts.Model

module Trainer =

    /// Generates training data using the given model.
    let private generateSamples model =
        OpenDeal.generate
            settings.Random
            settings.NumTraversals
            (fun deal ->
                Traverse.traverse deal model)
                |> Array.concat

    /// Trains the given advantage model using the given samples.
    let private trainAdvantageModel samples model =
        let stopwatch = Stopwatch.StartNew()
        let losses = AdvantageModel.train samples model
        if settings.Verbose then
            stopwatch.Stop()
            printfn $"Trained model in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float samples.Length} ms/sample)"
        losses

    /// Trains a new model using the given model.
    let private updateModel iter model =

            // generate training data from existing model
        let stopwatch = Stopwatch.StartNew()
        let samples = generateSamples model
        if settings.Verbose then
            printfn $"\n{samples.Length} samples generated in {stopwatch.Elapsed}"

            // train a new model
        model.Dispose()
        let model = new AdvantageModel()
        let losses = trainAdvantageModel samples model
        Path.Combine(
            settings.ModelDirPath,
            $"AdvantageModel%03d{iter}.pt")
                |> model.save
                |> ignore

            // log losses
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
                Trickster.player
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

        let rec loop deal =
            seq {
                let play, sampleOpt =
                    let hand =
                        let seat = OpenDeal.currentPlayer deal
                        deal.UnplayedCardMap[seat]
                    let legalPlays =
                        ClosedDeal.legalPlays hand deal.ClosedDeal
                            |> Seq.toArray
                    if legalPlays.Length = 1 then
                        Array.exactlyOne legalPlays,
                        None
                    else
                        let play =
                            Trickster.player.Play hand deal.ClosedDeal
                        let regrets =
                            let strategy =
                                [|
                                    for card in legalPlays do
                                        if card = play then 1.0f
                                        else 0.0f
                                |]
                            let mean = Array.average strategy
                            strategy
                                |> Array.map (fun x -> x - mean)
                                |> DenseVector.ofArray
                                |> Strategy.toWide legalPlays
                        play,
                        AdvantageSample.create
                            hand deal.ClosedDeal regrets
                            |> Some

                match sampleOpt with
                    | Some sample -> yield sample
                    | None -> ()

                let deal = OpenDeal.addPlay play deal
                match Game.tryUpdateScore deal Score.zero with
                    | Some _ -> ()
                    | None -> yield! loop deal
            }

        OpenDeal.generate
            settings.Random
            numDeals
            (loop >> Seq.toArray)
                |> Array.concat

    let trainDirect numDeals =

        printfn $"{settings}"
        printfn $"numDeals: {numDeals}"
        let samples =
            createTrainingData numDeals
                |> Seq.toArray
        printfn $"Number of samples: {samples.Length}"

        let model = new AdvantageModel()
        let losses = trainAdvantageModel samples model
        printfn $"Final loss {Array.last losses}"

            // log losses
        for epoch = 0 to losses.Length - 1 do
            settings.Writer.add_scalar(
                $"advantage loss",
                losses[epoch], epoch)

        let challenger = createChallenger (
            Strategy.getFromAdvantage model)
        let avgPayoff =
            Tournament.run
                settings.Random
                Trickster.player
                challenger
        settings.Writer.add_scalar(
            $"advantage tournament", avgPayoff, 0)
