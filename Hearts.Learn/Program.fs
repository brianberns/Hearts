namespace Hearts.Learn

open System

module Program =

    Console.OutputEncoding <- System.Text.Encoding.UTF8
    Trainer.train () |> ignore
    // Direct.train 20_000
