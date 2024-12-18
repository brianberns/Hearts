namespace Hearts.DeepCfr

open System.Runtime

module Program =

    let run () =
        let stratModel = Trainer.train ()
        ()

    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    printfn $"Server garbage collection: {GCSettings.IsServerGC}\n"

    run ()
    // Trainer.trainDirect 20_000
