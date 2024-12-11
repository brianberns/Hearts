namespace Hearts.DeepCfr

module Program =

    let run () =
        let stratModel = Trainer.train ()
        ()

    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    Trainer.trainDirect 1000
