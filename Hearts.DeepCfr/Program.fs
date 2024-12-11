namespace Hearts.DeepCfr

module Program =

    let run () =
        let stratModel = Trainer.train ()
        ()

    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    for loss in Trainer.trainDirect 10000 do
        printfn "%A" loss
