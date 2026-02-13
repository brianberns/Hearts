namespace Hearts.Learn

open System
open System.Runtime
open System.Text

module Program =

    let run () =
        let settings =
            let writer = TensorBoard.createWriter ()
            Settings.create writer
        Settings.write settings
        if settings.Verbose then
            printfn $"Server garbage collection: {GCSettings.IsServerGC}"
            printfn $"Settings: {settings}"
        Trainer.train settings |> ignore

    let test () =
        let sample =
            let encoding = Array.zeroCreate Hearts.Model.Model.inputSize
            let regrets = MathNet.Numerics.LinearAlgebra.DenseVector.create Hearts.Model.Model.outputSize 1.0f
            AdvantageSample.create encoding regrets 1
        do
            use store = AdvantageSampleStore.openOrCreate("AdvantageSamples.bin")
            assert(store.Count = 0)
            AdvantageSampleStore.writeSamples [|sample|] store
        do 
            use store = AdvantageSampleStore.openOrCreate("AdvantageSamples.bin")
            assert(store.Count = 1)
            let sample' = store[0]
            assert(sample' = sample)

    Console.OutputEncoding <- Encoding.UTF8
    test ()
