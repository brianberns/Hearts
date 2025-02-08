namespace Hearts.Learn

open System

module Program =

    Console.OutputEncoding <- System.Text.Encoding.UTF8
    Trainer.train () |> ignore
    // Trainer.trainDirect 20_000
