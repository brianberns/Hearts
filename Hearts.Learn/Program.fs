namespace Hearts.Learn

open System
open System.Runtime
open System.Text

open TorchSharp
open Hearts.Model

module Program =

    Console.OutputEncoding <- Encoding.UTF8

    let settings =
        {
            NumDealsPerIteration = 30_000
            DealBatchSize = 200
            SampleBranchRate = 0.2
            SampleReservoirCapacity = 40_000_000
            HiddenSize = Encoding.encodedLength * 2
            NumHiddenLayers = 9
            NumTrainingEpochs = 400
            TrainingBatchSize = 100_000
            TrainingSubBatchSize = 25_000
            DropoutRate = 0.3
            LearningRate = 1e-3
            NumIterations = 50
            NumEvaluationDeals = 20000
            Device = torch.CUDA
            ModelDirPath = "./Models"
            Writer = TensorBoard.writer
            Verbose = true
        }

    if settings.Verbose then
        printfn $"Server garbage collection: {GCSettings.IsServerGC}"
        printfn $"Settings: {settings}"

    Trainer.train settings |> ignore
