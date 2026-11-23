namespace Hearts.Train

open System.Diagnostics

open Hearts
open Hearts.Heuristic
open Hearts.Learn
open Hearts.Model

module Trainer =

    /// Evaluates the given model by playing it against a
    /// standard.
    let private evaluate settings iteration model =

        let winRate =
            Tournament.run
                0
                false             // avoid cross-thread TorchSharp problems (memory leaks, toFloat crash)
                settings.NumEvaluationGames
                Claude.player
                (Strategy.createPlayer model)

        if settings.Verbose then
            printfn $"Tournament win rate: %0.5f{winRate}"
        settings.Writer.add_scalar(
            $"tournament win rate", winRate, iteration)

    /// Uses stored samples to train a new model.
    let trainModel settings (sampleStores : AdvantageSampleStoreGroup) =

            // train new model
        let stopwatch = Stopwatch.StartNew()
        use model =
            new AdvantageModel(
                settings.HiddenSize,
                settings.NumHiddenLayers,
                settings.DropoutRate,
                settings.Device)
        AdvantageModel.train settings sampleStores model
        stopwatch.Stop()

           // save the model
        if settings.Verbose then
            printfn $"Trained model on {sampleStores.NumSamples} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float sampleStores.NumSamples} ms/sample)"

            // evaluate model
        evaluate settings sampleStores.Iteration model
