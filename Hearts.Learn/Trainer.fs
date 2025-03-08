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

    /// Initial advantage state.
    let empty =
        {
            ModelOpt = None
            Reservoir =
                Reservoir.create
                    settings.Random
                    settings.NumAdvantageSamples
        }

module Trainer =

    /// Generates training data using the given model.
    let private generateSamples iter modelOpt =

        let mutable count = 0     // ugly, but just for logging
        let lockable = new obj()

            // move model to CPU for faster inference
        modelOpt
            |> Option.iter (fun (model : AdvantageModel) ->
                model.MoveTo(torch.CPU))

            // function to get strategy for a given info set
        let getStrategy =
            match modelOpt with
                | Some model ->
                    fun infoSet legalPlays ->
                        Strategy.getFromAdvantage
                            infoSet model legalPlays
                | None ->
                    fun _ legalPlays ->
                        Strategy.random legalPlays.Length

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

        let act infoSet =
            let actionType, legalActions =
                InformationSet.legalActions infoSet
            let strategy =
                Strategy.getFromAdvantage
                    infoSet model legalActions
            let action =
                lock settings.Random (fun () ->
                    Vector.sample settings.Random strategy)
                    |> Array.get legalActions
            actionType, action

        { Act = act }

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

        if settings.Verbose then
            printfn $"Model input size: {Network.inputSize}"
            printfn $"Model output size: {Network.outputSize}"

            // run the iterations
        let iterNums = seq { 1 .. settings.NumIterations }
        (AdvantageState.empty, iterNums)
            ||> Seq.fold (fun state iter ->
                trainIteration iter state)
