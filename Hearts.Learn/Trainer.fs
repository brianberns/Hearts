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

        /// Persistent reservoir of training data.
        Reservoir : PersistentReservoir

        /// Current iteration number.
        CurrentIteration : int

        /// Base seed for reproducibility.
        BaseSeed : int
    }

    interface IDisposable with

        /// Cleanup.
        member this.Dispose() =
            this.ModelOpt
                |> Option.iter _.Dispose()
            (this.Reservoir :> IDisposable).Dispose()

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

    /// Adds the given samples to the given reservoir and then
    /// uses the reservoir to train a new model.
    let private trainAdvantageModel settings iter samples state =

            // cache new training data
        PersistentReservoir.addMany samples state.Reservoir

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
        let reservoirItems = PersistentReservoir.items state.Reservoir
        AdvantageModel.train
            settings iter (Some eval) reservoirItems model
        stopwatch.Stop()
        let count = PersistentReservoir.count state.Reservoir
        if settings.Verbose then
            printfn $"Trained model on {count} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float count} ms/sample)"

        { state with
            ModelOpt = Some model
            CurrentIteration = iter
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
            float32 (PersistentReservoir.count state.Reservoir),
            iter)

        state

    /// Trains for the given number of iterations.
    let train settings =

        if settings.Verbose then
            printfn $"Model input size: {Model.inputSize}"
            printfn $"Model output size: {Model.outputSize}"

            // try to resume from checkpoint
        let startIter, state =
            match Checkpoint.tryLoadLatest settings.CheckpointDbPath with
            | Some checkpoint ->
                if settings.Verbose then
                    printfn $"Resuming from iteration {checkpoint.Iteration}"
                let model =
                    new AdvantageModel(
                        settings.HiddenSize,
                        settings.NumHiddenLayers,
                        settings.DropoutRate,
                        settings.Device)
                model.load(checkpoint.ModelPath) |> ignore
                model.eval()
                let reservoir = PersistentReservoir.openExisting settings.ReservoirPath
                checkpoint.Iteration + 1,
                {
                    ModelOpt = Some model
                    Reservoir = reservoir
                    CurrentIteration = checkpoint.Iteration
                    BaseSeed = checkpoint.BaseSeed
                }
            | None ->
                if settings.Verbose then
                    printfn "Starting fresh training"
                let baseSeed = Random().Next()
                1,
                {
                    ModelOpt = None
                    Reservoir =
                        PersistentReservoir.create
                            settings.ReservoirPath
                            baseSeed
                    CurrentIteration = 0
                    BaseSeed = baseSeed
                }

            // run the iterations
        let iterNums = seq { startIter .. settings.NumIterations }
        (state, iterNums)
            ||> Seq.fold (fun state iter ->
                if settings.Verbose then
                    printfn $"\n*** Iteration {iter} ***"
                let state = updateModel settings iter state

                    // save checkpoint
                PersistentReservoir.flush state.Reservoir
                let modelPath =
                    Path.Combine(
                        settings.ModelDirPath,
                        $"AdvantageModel%03d{iter}.pt")
                Checkpoint.save
                    settings.CheckpointDbPath
                    iter
                    (PersistentReservoir.numSeen state.Reservoir)
                    (PersistentReservoir.count state.Reservoir)
                    state.BaseSeed
                    modelPath

                Option.iter (
                    evaluate settings iter None) state.ModelOpt
                state)
