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
        ModelOpt : Option<AdvantageModel>

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
    let create capacity rng =
        {
            ModelOpt = None
            Reservoir = Reservoir.create rng capacity
        }

module Trainer =

    /// Generates training data using the given model.
    let private generateSamples settings iter modelOpt =

        settings.Writer.add_scalar(
            $"advantage samples/iter%03d{iter}",
            0f, 0)

        let chunkSize = settings.DealBatchSize
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

    /// Adds the given samples to the given reservoir and then
    /// uses the reservoir to train a new model.
    let private trainAdvantageModel settings iter samples state =

            // cache new training data
        let resv =
            Reservoir.addMany samples state.Reservoir

            // train new model
        let stopwatch = Stopwatch.StartNew()
        let model =
            new AdvantageModel(
                settings.HiddenSize,
                settings.NumHiddenLayers,
                settings.DropoutRate,
                settings.Device)
        AdvantageModel.train settings iter resv.Items model
        stopwatch.Stop()
        if settings.Verbose then
            printfn $"Trained model on {resv.Items.Count} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float resv.Items.Count} ms/sample)"

        {
            Reservoir = resv
            ModelOpt = Some model
        }

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
                settings.ModelDirPath
                    |> Directory.CreateDirectory
                    |> ignore
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

    /// Evaluates the given model by playing it against a
    /// standard.
    let evaluate settings iter (model : AdvantageModel) =
        let score, payoff =
            Tournament.run
                (Random(0))       // use repeatable test set, not seen during training
                false             // avoid cross-thread TorchSharp problems (memory leaks, toFloat crash)
                settings.NumEvaluationDeals
                Trickster.player
                (Strategy.createPlayer model)
        if settings.Verbose then
            printfn "\nTournament:"
            for (seat, points) in Score.indexed score do
                printfn $"   %-6s{string seat}: {points}"
            printfn $"   Payoff: %0.5f{payoff}"
        settings.Writer.add_scalar(
            $"advantage tournament", payoff, iter)

    /// Trains for the given number of iterations.
    let train settings =

        if settings.Verbose then
            printfn $"Model input size: {Model.inputSize}"
            printfn $"Model output size: {Model.outputSize}"

            // run the iterations
        let state =
            AdvantageState.create
                settings.SampleReservoirCapacity
                (Random())
        let iterNums = seq { 1 .. settings.NumIterations }
        (state, iterNums)
            ||> Seq.fold (fun state iter ->
                if settings.Verbose then
                    printfn $"\n*** Iteration {iter} ***"
                let state = updateModel settings iter state   // create new model
                Option.iter (
                    evaluate settings iter) state.ModelOpt    // evaluate model
                state)
