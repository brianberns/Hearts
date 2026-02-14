namespace Hearts.Learn

open System
open System.Runtime
open System.Text

open Hearts.Model

module Program =

    let run () =
        let settings =
            let writer = TensorBoard.createWriter ()
            Settings.create writer
        Settings.write settings
        if settings.Verbose then
            printfn $"Server garbage collection: {GCSettings.IsServerGC}"
            printfn $"Settings: {settings}"
            printfn $"Model input size: {Model.inputSize}"
            printfn $"Model output size: {Model.outputSize}"
        Trainer.train settings |> ignore

    Console.OutputEncoding <- Encoding.UTF8
    run ()
