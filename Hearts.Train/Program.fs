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
            printfn $"Server garbage collection: {GCSettings.IsServerGC}"
            printfn $"Settings: {settings}"
            printfn $"Model input size: {Model.inputSize}"
            printfn $"Model output size: {Model.outputSize}"

        let sampleStore =
            let file =
                DirectoryInfo(settings.ModelDirPath)
                    .GetFiles("*.bin")
                    |> Array.exactlyOne
            AdvantageSampleStore.openRead file.FullName
        Trainer.trainModel settings sampleStore
            |> ignore

    Console.OutputEncoding <- Encoding.UTF8
    run ()
