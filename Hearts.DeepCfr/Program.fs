namespace Hearts.DeepCfr

module Program =

    let run () =
        let stratModel = Trainer.train ()
        ()

    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    run ()
    // Trainer.trainDirect 20_000
