namespace Hearts.Train

open System
open System.IO
open System.Runtime
open System.Text

open Hearts.Learn
open Hearts.Model

module Program =

    let run () =

            // get settings
        let settings =
            let writer = TensorBoard.createWriter ()
            Settings.create writer
        Settings.write settings
        if settings.Verbose then
            printfn "Settings:"
            printfn $"   Server garbage collection: {GCSettings.IsServerGC}"
            printfn $"   Hidden size: {settings.HiddenSize}"
            printfn $"   # hidden layers: {settings.NumHiddenLayers}"
            printfn $"   # training epochs: {settings.NumTrainingEpochs}"
            printfn $"   # epochs/evaluation: {settings.NumEpochsPerEvaluation}"
            printfn $"   Training batch size: {settings.TrainingBatchSize}"
            printfn $"   Training sub-batch size: {settings.TrainingSubBatchSize}"
            printfn $"   Dropout rate: {settings.DropoutRate}"
            printfn $"   Learning rate: {settings.LearningRate}"
            printfn $"   # evaluation deals: {settings.NumEvaluationDeals}"
            printfn $"   Device: {settings.Device}"
            printfn $"   Model directory: {settings.ModelDirPath}"
            printfn $"   Model input size: {Model.inputSize}"
            printfn $"   Model output size: {Model.outputSize}"

            // get training data
        let sampleStore =
            let file =
                DirectoryInfo(settings.ModelDirPath)
                    .GetFiles("*.bin")
                    |> Array.exactlyOne
            AdvantageSampleStore.openRead file.FullName
        if settings.Verbose then
            printfn $"Sample store: {Path.GetFileName(sampleStore.Stream.Name)}: {sampleStore.Count} samples"

            // train model
        let modelPath = Trainer.trainModel settings sampleStore
        if settings.Verbose then
            printfn $"Created model: {modelPath}"

    Console.OutputEncoding <- Encoding.UTF8
    run ()
