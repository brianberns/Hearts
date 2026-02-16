namespace Hearts.Generate

open System
open System.Diagnostics
open System.Runtime
open System.Text

open Hearts.Learn

module Program =

    let run iteration modelPath =

        let settings =
            let writer = TensorBoard.createWriter ()
            Settings.create writer
        Settings.write settings
        if settings.Verbose then
            printfn $"Server garbage collection: {GCSettings.IsServerGC}"

        let stopwatch = Stopwatch.StartNew()
        let numSamples = Trainer.generateSamples settings -1 state
        if settings.Verbose then
            printfn $"\n{numSamples} samples generated in {stopwatch.Elapsed}"

    Console.OutputEncoding <- Encoding.UTF8
    run ()
