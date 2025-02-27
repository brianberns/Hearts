namespace Hearts.Learn

open System
open System.Diagnostics
open System.IO

open MathNet.Numerics.LinearAlgebra

open TorchSharp

open Hearts
open Hearts.Model

module Trainer =

    /// Advantage state.
    type AdvantageState =
        {
            /// Current model.
            ModelOpt : Option<AdvantageModel>

            /// Reservoir of training data.
            Reservoir : Reservoir<AdvantageSample>
        }

        /// Cleanup.
        interface IDisposable with
            member this.Dispose() =
                this.ModelOpt
                    |> Option.iter _.Dispose()

    module AdvantageState =

        /// Creates an initial advantage state.
        let create device =
            {
                ModelOpt = None
                Reservoir =
                    Reservoir.create
                        settings.Random
                        settings.NumAdvantageSamples
            }

    /// Generates training data using the given model.
    let private generateSamples iter modelOpt =

        let mutable count = 0     // ugly, but just for logging
        let lockable = new obj()

        modelOpt                  // faster inference on CPU
            |> Option.iter (fun (model : AdvantageModel) ->
                model.MoveTo(torch.CPU))

        let getStrategy =
            match modelOpt with
                | Some model ->
                    Strategy.getFromAdvantage model
                | None ->
                    fun _ _ legalPlays ->
                        let n = legalPlays.Length
                        DenseVector.create n (1.0f / float32 n)

        OpenDeal.generate
            settings.Random
            settings.NumTraversals
            (fun deal ->

                let samples =
                    Traverse.traverse iter deal getStrategy

                lock lockable (fun () ->
                    count <- count + 1
                    settings.Writer.add_scalar(
                        $"advantage samples/iter%03d{iter}",
                        float32 samples.Length,
                        count))

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
        let model = new AdvantageModel(settings.Device)
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

        let play hand deal =
            let legalPlays =
                deal
                    |> ClosedDeal.legalPlays hand
                    |> Seq.toArray
            let strategy =
                Strategy.getFromAdvantage model
                    hand deal legalPlays
            lock settings.Random (fun () ->
                Vector.sample settings.Random strategy)
                |> Array.get legalPlays

        { Play = play }

    /// Evaluates the given model by playing it against a
    /// standard.
    let private evaluate iter (model : AdvantageModel) =

        model.MoveTo(torch.CPU)               // faster inference on CPU

        let avgPayoff =
            Tournament.run
                (Random(Settings.seed + 1))   // use repeatable test set, not seen during training
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

            // create initial state
        let state = AdvantageState.create torch.CPU
        if settings.Verbose then
            printfn $"Model input size: {Network.inputSize}"
            printfn $"Model hidden size: {Network.hiddenSize}"
            printfn $"Model output size: {Network.outputSize}"

            // run the iterations
        let iterNums = seq { 1 .. settings.NumIterations }
        (state, iterNums)
            ||> Seq.fold (fun state iter ->
                trainIteration iter state)
