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
        /// Current model, if any.
        ModelOpt : Option<AdvantageModel>

        /// Stored training data.
        SampleStore : AdvantageSampleStore
    }

    /// Cleanup.
    member this.Dispose() =
        this.ModelOpt
            |> Option.iter _.Dispose()
        this.SampleStore.Dispose()

    interface IDisposable with

        /// Cleanup.
        member this.Dispose() =
            this.Dispose()

module AdvantageState =

    let create modelOpt sampleStore =
        {
            ModelOpt = modelOpt
            SampleStore = sampleStore
        }

module Trainer =

    /// Evaluates the given model by playing it against a
    /// standard.
    let evaluate settings iter epochOpt model =

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

    /// Uses stored samples to train a new model.
    let private trainAdvantageModel settings iter sampleStore =

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
            settings iter (Some eval) sampleStore model
        stopwatch.Stop()

           // save the model
        if settings.Verbose then
            printfn $"Trained model on {sampleStore.Count} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float sampleStore.Count} ms/sample)"
        Path.Combine(
            settings.ModelDirPath,
            $"AdvantageModel%03d{iter}.pt")
                |> model.save
                |> ignore

        model

    /// Trains for the given number of iterations.
    let train settings =

        /// TensorBoard logging.
        let log count iter =
            settings.Writer.add_scalar(
                "advantage sample store",
                float32 count, iter)

            // initialize training state
        let store =
            let iteration = -1
            Path.Combine(
                settings.ModelDirPath,
                $"AdvantageSamples.{iteration}.{DateTime.Now.Ticks}")
                |> AdvantageSampleStore.create iteration
                |> AdvantageState.create None

            // run the iterations
        let iterNums =
            let iter =
                state.RestartIterationOpt
                    |> Option.defaultValue 1
            seq { iter .. settings.NumIterations }
        (state, iterNums)
            ||> Seq.fold (fun state iter ->
                if settings.Verbose then
                    printfn $"\n*** Iteration {iter} ***"

                    // generate training data from existing model?
                if state.RestartIterationOpt.IsNone then
                    let stopwatch = Stopwatch.StartNew()
                    let numSamples = generateSamples settings iter state
                    log state.SampleStore.Count iter
                    if settings.Verbose then
                        printfn $"\n{numSamples} samples generated in {stopwatch.Elapsed}"

                    // train and evaluate new model
                let model =
                    trainAdvantageModel settings iter state.SampleStore
                evaluate settings iter None model
                { state with
                    ModelOpt = Some model
                    RestartIterationOpt = None })
