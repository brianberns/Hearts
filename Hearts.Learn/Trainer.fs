namespace Hearts.Learn

open System
open System.Diagnostics
open System.IO

open Hearts
open Hearts.Heuristic
open Hearts.Model

/// Advantage state.
type AdvantageState =
    {
        /// Current model.
        ModelOpt : Option<AdvantageModel>

        /// Stored training data.
        SampleStore : AdvantageSampleStore
    }

    interface IDisposable with

        /// Cleanup.
        member this.Dispose() =
            this.ModelOpt
                |> Option.iter _.Dispose()
            this.SampleStore.Dispose()

module AdvantageState =

    let private storeFileName = "AdvantageSamples.bin"

    let init modelDirPath =
        let path = Path.Combine(modelDirPath, storeFileName)
        let store = AdvantageSampleStore.openOrCreate path
        {
            ModelOpt = None
            SampleStore = store
        }

module Trainer =

    /// Generates training data using the given model.
    let private generateSamples settings iter modelOpt =

        settings.Writer.add_scalar(
            $"advantage samples/iter%03d{iter}",
            0f, 0)

        let chunkSize = settings.DealBatchSize
        let samples =
            Array.zeroCreate<int> settings.NumDealsPerIteration
                |> Array.chunkBySize chunkSize
                |> Array.indexed
                |> Array.collect (fun (i, chunk) ->

                    let samples =
                        OpenDeal.playDeals
                            (Random())
                            true
                            chunk.Length
                            (fun deal ->
                                let rng = Random()   // each thread has its own RNG
                                Traverse.traverse settings iter deal rng)
                            |> Inference.complete
                                settings.TrainingSubBatchSize   // reuse setting for number of deals per inference chunk
                                modelOpt
                    GC.Collect()   // clean up continuations

                    settings.Writer.add_scalar(
                        $"advantage samples/iter%03d{iter}",
                        float32 samples.Length / float32 chunkSize,
                        (i + 1) * chunkSize)

                    samples)

        settings.Writer.add_scalar(
            "advantage samples",
            float32 samples.Length, iter)

        samples

    /// Evaluates the given model by playing it against a
    /// standard.
    let evaluate settings iter epochOpt (model : AdvantageModel) =

        let payoff =
            Tournament.run
                0
                false             // avoid cross-thread TorchSharp problems (memory leaks, toFloat crash)
                settings.NumEvaluationDeals
                Claude.player
                (Strategy.createPlayer model)

        match epochOpt with

            | Some epoch ->
                settings.Writer.add_scalar(
                    $"advantage tournament/iter%03d{iter}",
                    payoff, epoch)

            | None ->
                if settings.Verbose then
                    printfn $"Tournament payoff: %0.5f{payoff}"
                settings.Writer.add_scalar(
                    $"advantage tournament", payoff, iter)

    /// Adds the given samples to the sample store and then
    /// uses all the stored samples to train a new model.
    let private trainAdvantageModel settings iter samples state =

            // save new training data
        AdvantageSampleStore.writeSamples samples state.SampleStore

            // train new model
        let stopwatch = Stopwatch.StartNew()
        let model =
            new AdvantageModel(
                settings.HiddenSize,
                settings.NumHiddenLayers,
                settings.DropoutRate,
                settings.Device)
        let eval epoch model =
            evaluate settings iter (Some epoch) model
        AdvantageModel.train
            settings iter (Some eval) state.SampleStore.Samples model
        stopwatch.Stop()
        if settings.Verbose then
            printfn $"Trained model on {state.SampleStore.Count} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float state.SampleStore.Count} ms/sample)"

        { state with ModelOpt = Some model }

    /// Trains a new model using the given model.
    let private updateModel settings iter state =

            // generate training data from existing model
        let stopwatch = Stopwatch.StartNew()
        let samples =
            generateSamples settings iter state.ModelOpt
        if settings.Verbose then
            printfn $"\n{samples.Length} samples generated in {stopwatch.Elapsed}"

            // train a new model on GPU
        let state =
            trainAdvantageModel settings iter samples state

           // save the model
        state.ModelOpt
            |> Option.iter (fun model ->
                Path.Combine(
                    settings.ModelDirPath,
                    $"AdvantageModel%03d{iter}.pt")
                        |> model.save
                        |> ignore)

        settings.Writer.add_scalar(
            $"advantage sample store",
            float32 state.SampleStore.Count,
            iter)

        state

    /// Trains for the given number of iterations.
    let train settings =

        if settings.Verbose then
            printfn $"Model input size: {Model.inputSize}"
            printfn $"Model output size: {Model.outputSize}"

            // run the iterations
        let state =
            AdvantageState.init settings.ModelDirPath
        let iterNums = seq { 1 .. settings.NumIterations }
        (state, iterNums)
            ||> Seq.fold (fun state iter ->
                if settings.Verbose then
                    printfn $"\n*** Iteration {iter} ***"
                let state = updateModel settings iter state
                Option.iter (
                    evaluate settings iter None) state.ModelOpt
                state)
