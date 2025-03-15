namespace Hearts.Learn

open System
open System.Diagnostics
open System.IO

open TorchSharp

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
    let create rng =
        {
            ModelOpt = None
            Reservoir =
                Reservoir.create rng
                    settings.NumAdvantageSamples
        }

module Trainer =

    let generateRandomSamples iter =

        let mutable count = 0     // ugly, but just for logging

            // function to get strategy for a given info set
        let getStrategy infoSet =
            async {
                return Strategy.random infoSet.LegalActions.Length
            }

        let rng = Random()
        OpenDeal.generate
            rng
            settings.NumTraversals
            (fun deal ->

                let samples =
                    Traverse.traverse iter deal rng getStrategy
                        |> Async.RunSynchronously

                count <- count + 1
                settings.Writer.add_scalar(
                    $"advantage samples/iter%03d{iter}",
                    float32 samples.Length,
                    count)

                samples)
                |> Array.concat

    /// Generates training data using the given model.
    let private generateSamples iter (model : AdvantageModel) =

        let mutable count = 0     // ugly, but just for logging

            // move model to CPU for faster inference
        model.MoveTo(torch.CPU)

            // function to get strategy for a given info set
        let mgr = InferenceManager(model)
        let getStrategy infoSet : Async<MathNet.Numerics.LinearAlgebra.Vector<_>> =
            let idx = mgr.AddRequest(infoSet)
            Async.FromContinuations(fun (success, _error, _cancel) ->
                success (mgr.GetResponse(idx)))

        let rng = Random()
        OpenDeal.generate
            rng
            settings.NumTraversals
            (fun deal ->

                let asyncSamples =
                    let rng = Random()   // each thread has its own RNG
                    Traverse.traverse iter deal rng getStrategy
                mgr.Infer()
                let samples =
                    Async.RunSynchronously asyncSamples

                count <- count + 1
                settings.Writer.add_scalar(
                    $"advantage samples/iter%03d{iter}",
                    float32 samples.Length,
                    count)

                samples)
                |> Array.concat

    /// Adds the given samples to the given reservoir and then
    /// uses the reservoir to train a new model.
    let private trainAdvantageModel iter samples state =

            // cache new training data
        let resv =
            Reservoir.addMany samples state.Reservoir

            // train new model
        let stopwatch = Stopwatch.StartNew()
        let model =
            new AdvantageModel(
                settings.HiddenSize,
                settings.Device)
        AdvantageModel.train iter resv.Items model
        stopwatch.Stop()
        if settings.Verbose then
            printfn $"Trained model on {resv.Items.Count} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float resv.Items.Count} ms/sample)"

        {
            Reservoir = resv
            ModelOpt = Some model
        }

    /// Trains a new model using the given model.
    let private updateModel iter state =

            // generate training data from existing model
        let stopwatch = Stopwatch.StartNew()
        let samples =
            match state.ModelOpt with
                | Some model -> generateSamples iter model
                | None -> generateRandomSamples iter
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
                Strategy.getFromAdvantage [|infoSet|] model
                    |> Array.exactlyOne
            let action =
                Vector.sample rng strategy
                    |> Array.get infoSet.LegalActions
            infoSet.LegalActionType, action

        { Act = act }

    /// Evaluates the given model by playing it against a
    /// standard.
    let private evaluate iter (model : AdvantageModel) =

        model.MoveTo(torch.CPU)   // faster inference on CPU

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
