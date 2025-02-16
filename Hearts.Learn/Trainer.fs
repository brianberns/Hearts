namespace Hearts.Learn

open System
open System.Diagnostics
open System.IO

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

        interface IDisposable with
            member this.Dispose() = this.Model.Dispose()

    module private AdvantageState =

        /// Creates an advantage state.
        let create () =
            {
                Model =
                    new AdvantageModel(settings.Device)
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
                    Model = new AdvantageModel(settings.Device)
            }

    /// Generates training data using the given model.
    let private generateSamples iter model =

        let mutable count = 0   // ugly, but just for logging
        let lockable = new obj()

        OpenDeal.generate
            settings.Random
            settings.NumTraversals
            (fun deal ->

                let samples =
                    Traverse.traverse iter deal model

                lock lockable (fun () ->
                    count <- count + 1
                    settings.Writer.add_scalar(
                        $"advantage samples/iter%03d{iter}",
                        float32 samples.Length,
                        count))

                samples)
                |> Array.concat

    /// Adds the given samples to the given reservoir and then
    /// uses the reservoir to train the given model.
    let private trainAdvantageModel iter samples state =

        let resv =
            Reservoir.addMany samples state.Reservoir

        let stopwatch = Stopwatch.StartNew()
        AdvantageModel.train
            iter resv.Items state.Model
        if settings.Verbose then
            stopwatch.Stop()
            printfn $"Trained model on {resv.Items.Count} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float resv.Items.Count} ms/sample)"
        { state with Reservoir = resv }

    /// Trains a new model using the given model.
    let private updateModel iter state =

            // generate training data from existing model
        let stopwatch = Stopwatch.StartNew()
        let samples = generateSamples iter state.Model
        if settings.Verbose then
            printfn $"\n{samples.Length} samples generated in {stopwatch.Elapsed}"

            // train a new model
        let state =
            AdvantageState.resetModel state
                |> trainAdvantageModel iter samples
        Path.Combine(
            settings.ModelDirPath,
            $"AdvantageModel%03d{iter}.pt")
                |> state.Model.save
                |> ignore
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
            strategy
                |> Vector.sample settings.Random
                |> Array.get legalPlays

        { Play = play }

    /// Evaluates the given model by playing it against a
    /// standard.
    let private evaluate iter model =
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
        evaluate iter state.Model
        state

    /// Trains for the given number of iterations.
    let train () =

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
            printfn $"Model hidden size: {Network.hiddenSize}"
            printfn $"Model output size: {Network.outputSize}"
            printfn $"Model parameter count: {nParms}"

            // evaluate initial model
        evaluate 0 state.Model

            // run the iterations
        let iterNums = seq { 1 .. settings.NumIterations }
        let state =
            (state, iterNums)
                ||> Seq.fold (fun state iter ->
                    trainIteration iter state)
        state.Model
