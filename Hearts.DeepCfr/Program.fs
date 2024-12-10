namespace Hearts.DeepCfr

module Program =

    // let stratModel = Trainer.train ()
    // ()

    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    for tuple in Trainer.trainDirect 1 do
        printfn "%A" tuple
