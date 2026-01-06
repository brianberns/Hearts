namespace Hearts.Learn

open System
open System.Runtime
open System.Text

module Program =

    let run () =
        let settings =
            let writer = TensorBoard.createWriter ()
            Settings.create writer
        if settings.Verbose then
            printfn $"Server garbage collection: {GCSettings.IsServerGC}"
            printfn $"Settings: {settings}"
        Trainer.train settings |> ignore

    let search () =
        for learningRate in [ 0.0001; 0.0003; 0.001; 0.003 ] do
            for dropoutRate in [ 0.1; 0.2; 0.3; 0.4 ] do
                for numHiddenLayers in [ 2; 4; 6; 8 ] do
                    for hiddenSize in [ 500; 1000; 1500; 2000 ] do
                        let settings =
                            let writer = TensorBoard.createWriter ()
                            { Settings.create writer with
                                NumDealsPerIteration = 1000
                                SampleBranchRate = 0.17
                                SampleReservoirCapacity = 1_000_000
                                NumIterations = 5
                                NumTrainingEpochs = 500
                                NumHiddenLayers = numHiddenLayers
                                HiddenSize = hiddenSize
                                DropoutRate = dropoutRate
                                LearningRate = learningRate }
                        if settings.Verbose then
                            printfn ""
                            printfn $"Server garbage collection: {GCSettings.IsServerGC}"
                            printfn $"Settings: {settings}"
                        Trainer.train settings |> ignore

    Console.OutputEncoding <- Encoding.UTF8
    search ()
