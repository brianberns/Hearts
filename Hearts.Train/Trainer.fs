namespace Hearts.Train

open System.Diagnostics
open System.IO

open Hearts
open Hearts.Heuristic
open Hearts.Learn
open Hearts.Model

module Trainer =

    /// Evaluates the given model by playing it against a
    /// standard.
    let private evaluate settings iteration epochOpt model =

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
                    $"advantage tournament/iter%03d{iteration}",
                    payoff, epoch)

            | None ->
                if settings.Verbose then
                    printfn $"Tournament payoff: %0.5f{payoff}"
                settings.Writer.add_scalar(
                    $"advantage tournament", payoff, iteration)

    /// Uses stored samples to train a new model.
    let trainModel settings sampleStore =

            // train new model
        let stopwatch = Stopwatch.StartNew()
        let model =
            new AdvantageModel(
                settings.HiddenSize,
                settings.NumHiddenLayers,
                settings.DropoutRate,
                settings.Device)
        let eval epoch model =
            evaluate settings sampleStore.Iteration (Some epoch) model
        AdvantageModel.train
            settings (Some eval) sampleStore model
        stopwatch.Stop()

           // save the model
        if settings.Verbose then
            printfn $"Trained model on {sampleStore.Count} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float sampleStore.Count} ms/sample)"
        Path.Combine(
            settings.ModelDirPath,
            $"AdvantageModel%03d{sampleStore.Iteration}.pt")
                |> model.save
                |> ignore

        model
