namespace Hearts.Learn

open System

module Program =

    let run () =
        let model = Trainer.train ()
        ()

    Console.OutputEncoding <- System.Text.Encoding.UTF8
    run ()
    // Trainer.trainDirect 20_000
