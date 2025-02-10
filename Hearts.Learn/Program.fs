﻿namespace Hearts.Learn

open System

module Program =

    Console.OutputEncoding <- System.Text.Encoding.UTF8

    if settings.Verbose then
        printfn $"Server garbage collection: {System.Runtime.GCSettings.IsServerGC}"
        printfn $"Settings: {settings}"

    Trainer.train () |> ignore
    // Direct.train 20_000
