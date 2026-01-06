namespace Hearts.Learn

open System
open System.Runtime
open System.Text

module Program =
    Console.OutputEncoding <- Encoding.UTF8
    let settings =
        let writer = TensorBoard.createWriter ()
        Settings.create writer
    if settings.Verbose then
        printfn $"Server garbage collection: {GCSettings.IsServerGC}"
        printfn $"Settings: {settings}"
    Trainer.train settings |> ignore
