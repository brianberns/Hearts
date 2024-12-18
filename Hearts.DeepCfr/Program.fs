namespace Hearts.DeepCfr

open System.Runtime

module Program =

    let run () =
        let stratModel = Trainer.train ()
        ()

    printfn $"Server garbage collection: {GCSettings.IsServerGC}\n"
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    run ()
    // Trainer.trainDirect 20_000
