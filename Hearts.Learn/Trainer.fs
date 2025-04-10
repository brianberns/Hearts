namespace Hearts.Learn

open System
open System.Diagnostics
open System.IO

open Hearts
open Hearts.Model

/// Advantage state.
type AdvantageState =
    {
        /// Current model.
        ModelOpt : Option<HeartsModel>

        /// Reservoir of training data.
        Reservoir : Reservoir<AdvantageSample>
    }

    interface IDisposable with

        /// Cleanup.
        member this.Dispose() =
            this.ModelOpt
                |> Option.iter _.Dispose()

module AdvantageState =

    /// Creates an initial advantage state.
    let create rng =
        {
            ModelOpt = None
            Reservoir =
                Reservoir.create rng
                    settings.NumAdvantageSamples
        }

module Trainer =

    /// Generates training data using the given model.
    let private generateSamples iter modelOpt =

        settings.Writer.add_scalar(
            $"advantage samples/iter%03d{iter}",
            0f, 0)

        let chunkSize = settings.TraversalBatchSize
        Array.zeroCreate<int> settings.NumTraversals
            |> Array.chunkBySize chunkSize
            |> Array.indexed
            |> Array.collect (fun (i, chunk) ->

                let samples =
                    OpenDeal.generate
                        (Random())
                        chunk.Length
                        (fun deal ->
                            let rng = Random()   // each thread has its own RNG
                            Traverse.traverse iter deal rng)
                        |> Inference.complete modelOpt

                settings.Writer.add_scalar(
                    $"advantage samples/iter%03d{iter}",
                    float32 samples.Length / float32 chunkSize,
                    (i + 1) * chunkSize)

                samples)

    /// Adds the given samples to the given reservoir and then
    /// uses the reservoir to train a new model.
    let private trainModel iter samples state =

            // cache new training data
        let resv =
            Reservoir.addMany samples state.Reservoir

            // train exchange model
        let stopwatch = Stopwatch.StartNew()
        let exchangeModel =
            new ExchangeModel(
                settings.ExchangeHiddenSize,
                settings.NumHiddenLayers,
                settings.Device)
        AdvantageModel.trainQQQ
            iter resv.Items exchangeModel
        stopwatch.Stop()
        if settings.Verbose then
            printfn $"Trained exchange model on {resv.Items.Count} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float resv.Items.Count} ms/sample)"

            // train playout model
        stopwatch.Restart()
        let playoutModel =
            new PlayoutModel(
                settings.PlayoutHiddenSize,
                settings.NumHiddenLayers,
                settings.Device)
        AdvantageModel.trainQQQ
            iter resv.Items playoutModel
        stopwatch.Stop()
        if settings.Verbose then
            printfn $"Trained playout model on {resv.Items.Count} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float resv.Items.Count} ms/sample)"

        {
            Reservoir = resv
            ModelOpt =
                Some (
                    HeartsModel.create
                        exchangeModel playoutModel)
        }

    /// Trains a new model using the given model.
    let private updateModel iter state =

            // generate training data from existing model
        let stopwatch = Stopwatch.StartNew()
        let samples =
            generateSamples iter state.ModelOpt
        if settings.Verbose then
            printfn $"\n{samples.Length} samples generated in {stopwatch.Elapsed}"

            // train a new model on GPU
        let state =
            trainAdvantageModel iter samples state
        state.ModelOpt
            |> Option.iter (fun model ->
                Path.Combine(
                    settings.ModelDirPath,
                    $"AdvantageModel%03d{iter}.pt")
                        |> model.save
                        |> ignore)
        settings.Writer.add_scalar(
            $"advantage reservoir",
            float32 state.Reservoir.Items.Count,
            iter)

        state

    /// Creates a Hearts player using the given model.
    let createPlayer model =

        let rng = Random()   // each player has its own RNG

        let act infoSet =
            let strategy =
                Strategy.getFromAdvantage model [|infoSet|]
                    |> Array.exactlyOne
            let action =
                Vector.sample rng strategy
                    |> Array.get infoSet.LegalActions
            infoSet.LegalActionType, action

        { Act = act }

    /// Evaluates the given model by playing it against a
    /// standard.
    let private evaluate iter (model : AdvantageModel) =

        let avgPayoff =
            Tournament.run
                (Random(0))       // use repeatable test set, not seen during training
                Trickster.player
                (createPlayer model)
        settings.Writer.add_scalar(
            $"advantage tournament", avgPayoff, iter)

    /// Trains a single iteration.
    let private trainIteration iter state =
        if settings.Verbose then
            printfn $"\n*** Iteration {iter} ***"
        let state = updateModel iter state
        state.ModelOpt
            |> Option.iter (evaluate iter)
        state

    /// Trains for the given number of iterations.
    let train () =

        if settings.Verbose then
            printfn $"Model input size: {Network.inputSize}"
            printfn $"Model output size: {Network.outputSize}"

            // run the iterations
        let state = AdvantageState.create (Random())
        let iterNums = seq { 1 .. settings.NumIterations }
        (state, iterNums)
            ||> Seq.fold (fun state iter ->
                trainIteration iter state)
