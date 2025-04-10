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

        /// Reservoir of exchange training data.
        ExchangeReservoir : Reservoir<AdvantageSample>

        /// Reservoir of playout training data.
        PlayoutReservoir : Reservoir<AdvantageSample>
    }

    /// Cleanup.
    member this.Dispose() =
        this.ModelOpt
            |> Option.iter _.Dispose()

    interface IDisposable with

        /// Cleanup.
        member this.Dispose() = this.Dispose()

module AdvantageState =

    /// Creates an initial advantage state.
    let create rng =
        {
            ModelOpt = None
            ExchangeReservoir =
                Reservoir.create rng
                    settings.NumAdvantageSamples
            PlayoutReservoir =
                Reservoir.create rng
                    settings.NumAdvantageSamples
        }

module Trainer =

    /// Generates training data using the given model.
    let private generateSamples iter modelOpt =

        settings.Writer.add_scalar(
            $"samples/iter%03d{iter}/exchange",
            0f, 0)
        settings.Writer.add_scalar(
            $"samples/iter%03d{iter}/playout",
            0f, 0)

        let chunkSize = settings.TraversalBatchSize
        let passArrays, playArrays =
            Array.zeroCreate<int> settings.NumTraversals
                |> Array.chunkBySize chunkSize
                |> Array.indexed
                |> Array.map (fun (i, chunk) ->

                    let passSamples, playSamples =
                        OpenDeal.generate
                            (Random())
                            chunk.Length
                            (fun deal ->
                                let rng = Random()   // each thread has its own RNG
                                Traverse.traverse iter deal rng)
                            |> Inference.complete modelOpt

                    settings.Writer.add_scalar(
                        $"samples/iter%03d{iter}/exchange",
                        float32 passSamples.Length / float32 chunkSize,
                        (i + 1) * chunkSize)
                    settings.Writer.add_scalar(
                        $"samples/iter%03d{iter}/playout",
                        float32 playSamples.Length / float32 chunkSize,
                        (i + 1) * chunkSize)

                    passSamples, playSamples)

                |> Array.unzip

        Array.concat passArrays,
        Array.concat playArrays

    /// Adds the given samples to the given reservoir and then
    /// uses the reservoir to train a new model.
    let private trainAdvantageModel iter samples resv (model : Model) =

            // cache new training data
        let resv = Reservoir.addMany samples resv
        settings.Writer.add_scalar(
            $"reservoir/{model.GetName().ToLower()}",
            float32 resv.Items.Count,
            iter)

            // train model
        let stopwatch = Stopwatch.StartNew()
        AdvantageModel.train iter resv.Items model
        stopwatch.Stop()
        if settings.Verbose then
            printfn $"Trained {model.GetName().ToLower()} model on {resv.Items.Count} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float resv.Items.Count} ms/sample)"

        resv

    /// Adds the given samples to the given reservoir and then
    /// uses the reservoir to train a new model.
    let private trainModel iter passSamples playSamples state =

        let exchangeModel =
            new ExchangeModel(
                settings.ExchangeHiddenSize,
                settings.NumHiddenLayers,
                settings.Device)
        let exchangeResv =
            trainAdvantageModel
                iter passSamples state.ExchangeReservoir exchangeModel

        let playoutModel =
            new PlayoutModel(
                settings.PlayoutHiddenSize,
                settings.NumHiddenLayers,
                settings.Device)
        let playoutResv =
            trainAdvantageModel
                iter playSamples state.PlayoutReservoir playoutModel

        {
            ModelOpt =
                Some (
                    HeartsModel.create
                        exchangeModel playoutModel)
            ExchangeReservoir = exchangeResv
            PlayoutReservoir = playoutResv
        }

    /// Trains a new model using the given model.
    let private updateModel iter state =

            // generate training data from existing model
        let stopwatch = Stopwatch.StartNew()
        let passSamples, playSamples =
            generateSamples iter state.ModelOpt
        if settings.Verbose then
            printfn $"\n{passSamples.Length} exchange samples and {playSamples.Length} playout samples generated in {stopwatch.Elapsed}"

            // train a new model on GPU
        let state =
            trainModel iter passSamples playSamples state
        state.ModelOpt
            |> Option.iter (fun model ->
                Path.Combine(
                    settings.ModelDirPath,
                    $"ExchangeModel%03d{iter}.pt")
                        |> model.ExchangeModel.save
                        |> ignore
                Path.Combine(
                    settings.ModelDirPath,
                    $"PlayoutModel%03d{iter}.pt")
                        |> model.PlayoutModel.save
                        |> ignore)

        state

    /// Creates a Hearts player using the given model.
    let createPlayer model =

        let rng = Random()   // each player has its own RNG

        let act infoSet =
            let strategy =
                Strategy.getFromModel model [|infoSet|]
                    |> Array.exactlyOne
            let action =
                Vector.sample rng strategy
                    |> Array.get infoSet.LegalActions
            infoSet.LegalActionType, action

        { Act = act }

    /// Evaluates the given model by playing it against a
    /// standard.
    let private evaluate iter model =

        let avgPayoff =
            Tournament.run
                (Random(0))       // use repeatable test set, not seen during training
                Trickster.player
                (createPlayer model)
        settings.Writer.add_scalar(
            $"Tournament", avgPayoff, iter)

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
        let state = AdvantageState.create (Random())
        let iterNums = seq { 1 .. settings.NumIterations }
        (state, iterNums)
            ||> Seq.fold (fun state iter ->
                trainIteration iter state)
