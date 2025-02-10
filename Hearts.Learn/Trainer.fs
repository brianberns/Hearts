namespace Hearts.Learn

open System
open System.Diagnostics
open System.IO

open MathNet.Numerics.LinearAlgebra

open PlayingCards
open Hearts
open Hearts.Model

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

    /// Generates training data using the given model.
    let private generateSamples iter model =
        OpenDeal.generate
            settings.Random
            settings.NumTraversals
            (fun deal ->
                Traverse.traverse iter deal model)
                |> Array.concat

    /// Adds the given samples to the given reservoir and then
    /// uses the reservoir to train the given model.
    let private trainAdvantageModel samples state =

        let resv =
            Reservoir.addMany samples state.Reservoir

        let stopwatch = Stopwatch.StartNew()
        let losses =
            AdvantageModel.train resv.Items state.Model
        if settings.Verbose then
            stopwatch.Stop()
            printfn $"Trained model on {resv.Items.Count} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float resv.Items.Count} ms/sample)"
        losses

    /// Trains a new model using the given model.
    let private updateModel iter state =

            // generate training data from existing model
        let stopwatch = Stopwatch.StartNew()
        let samples = generateSamples iter state.Model
        if settings.Verbose then
            printfn $"\n{samples.Length} samples generated in {stopwatch.Elapsed}"

            // train a new model
        let state = AdvantageState.resetModel state
        let losses = trainAdvantageModel samples state
        Path.Combine(
            settings.ModelDirPath,
            $"AdvantageModel%03d{iter}.pt")
                |> state.Model.save
                |> ignore

            // log losses
        for epoch = 0 to losses.Length - 1 do
            settings.Writer.add_scalar(
                $"advantage loss/iter%03d{iter}",
                losses[epoch], epoch)

        state

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

    /// Evaluates the given model by playing it against
    /// a standard.
    let private evaluate iter model =
        let avgPayoff =
            let challenger =
                createChallenger (
                    Strategy.getFromAdvantage model)
            Tournament.run
                (Random(Settings.seed + 1))   // use same deals each iteration
                Trickster.player
                challenger
        settings.Writer.add_scalar(
            $"advantage tournament", avgPayoff, iter)

    /// Trains a single iteration.
    let private trainIteration iter state =
        if settings.Verbose then
            printfn $"\n*** Iteration {iter} ***"
        let state = updateModel iter state
        evaluate iter state.Model
        state

    /// Trains for the given number of iterations.
    let train () =

        if settings.Verbose then
            printfn $"Server garbage collection: {System.Runtime.GCSettings.IsServerGC}"
            printfn $"Settings: {settings}"

            // create initial state
        let state = AdvantageState.create ()
        let nParms =
            state.Model.parameters(true)
                |> Seq.where (fun parm -> parm.requires_grad)
                |> Seq.sumBy (fun parm -> parm.numel())
        settings.Writer.add_text(
            $"settings/AdvModelParmCount", string nParms, 0)
        if settings.Verbose then
            printfn $"Model input size: {Network.inputSize}"
            printfn $"Model output size: {Network.outputSize}"
            printfn $"Advantage model parameter count: {nParms}"

            // evaluate initial model
        evaluate 0 state.Model

            // run the iterations
        let iterNums = seq { 1 .. settings.NumIterations }
        let state =
            (state, iterNums)
                ||> Seq.fold (fun state iter ->
                    trainIteration iter state)
        state.Model

    /// Generates training data using a standard player.
    let private generateTrainingData numDeals =

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
                            hand deal.ClosedDeal regrets 1
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

    /// Trains a model directly.
    let trainDirect numDeals =

        printfn $"{settings}"
        printfn $"Number of deals: {numDeals}"

            // generate training data
        let samples =
            generateTrainingData numDeals
                |> Seq.toArray
        printfn $"Number of samples: {samples.Length}"

            // train model
        let model = new AdvantageModel()
        let losses = AdvantageModel.train samples model

            // log losses
        printfn $"Final loss {Array.last losses}"
        for epoch = 0 to losses.Length - 1 do
            settings.Writer.add_scalar(
                $"advantage loss",
                losses[epoch], epoch)

            // save trained model
        Path.Combine(
            settings.ModelDirPath,
            $"AdvantageModel.pt")
                |> model.save
                |> ignore

            // evaluate trained model
        let challenger = createChallenger (
            Strategy.getFromAdvantage model)
        let avgPayoff =
            Tournament.run
                settings.Random
                Trickster.player
                challenger
        settings.Writer.add_scalar(
            $"advantage tournament", avgPayoff, 0)
