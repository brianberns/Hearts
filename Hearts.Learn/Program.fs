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
        for learningRate in [ 0.001; 0.003; 0.01 ] do
            for dropoutRate in [ 0.1; 0.25; 0.4 ] do
                for numHiddenLayers in [ 2; 5; 8 ] do
                    for hiddenSize in [ 1000; 1500; 2000 ] do
                        let settings =
                            let writer = TensorBoard.createWriter ()
                            { Settings.create writer with
                                NumDealsPerIteration = 4000
                                SampleBranchRate = 0.17
                                SampleReservoirCapacity = 1_000_000
                                NumIterations = 4
                                NumTrainingEpochs = 500
                                NumHiddenLayers = numHiddenLayers
                                HiddenSize = hiddenSize
                                DropoutRate = dropoutRate
                                LearningRate = learningRate }
                        Settings.write settings
                        if settings.Verbose then
                            printfn ""
                            printfn $"Server garbage collection: {GCSettings.IsServerGC}"
                            printfn $"Settings: {settings}"
                        Trainer.train settings |> ignore

    Console.OutputEncoding <- Encoding.UTF8
    search ()
