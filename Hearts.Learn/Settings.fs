namespace Hearts.Learn

open System
open TorchSharp
open Hearts.Model

/// Hyperparameters.
type Settings =
    {
        /// Total number of games to create when generating sample
        /// data at the start of each iteration.
        NumGamesPerIteration : int

        /// Number of games to create per batch when generating
        /// sample data at the start of each iteration. E.g. 8000
        /// games at 200 games/batch is 40 batches.
        GameBatchSize : int

        /// Branch rate of the move tree when generating sample data
        /// at the start of each iteration. Larger values generate
        /// more samples.
        /// https://chatgpt.com/c/67b26aab-6504-8000-ba0e-0ae3c8a614ff
        SampleBranchRate : float

        /// Maximum number of samples to keep in the reservoir.
        SampleReservoirCapacity : int

        /// Input and output size of a hidden layer within the neural
        /// network.
        HiddenSize : int

        /// Number of hidden layers within the neural network.
        NumHiddenLayers : int

        /// Number of epochs to use when training the model.
        NumTrainingEpochs : int

        /// Batch size to use when training the model.
        TrainingBatchSize : int

        /// Sub-batch size to use when training the model.
        TrainingSubBatchSize : int

        /// Dropout rate to use when training the model.
        DropoutRate : float

        /// Optimizer learning rate to use when training the model.
        LearningRate : float

        /// Number of games to evaluate model after training.
        NumEvaluationGames : int

        /// Number of iterations to perform.
        NumIterations : int

        /// Device to use for training and running models.
        Device : torch.Device

        /// Tensorboard writer.
        Writer : Modules.SummaryWriter

        /// Path to directory where models will be saved.
        ModelDirPath : string

        /// Verbose output?
        Verbose : bool
    }

[<AutoOpen>]
module Settings =

    /// Tensorboard log writer.
    let private writer =
        let timespan = DateTime.Now - DateTime.Today
        torch.utils.tensorboard.SummaryWriter(
            $"runs/run%05d{int timespan.TotalSeconds}")

    /// Hyperparameters.
    let settings =

        let settings =
            {
                NumGamesPerIteration = 250
                GameBatchSize = 25
                SampleBranchRate = 0.17
                SampleReservoirCapacity = 100_000_000
                HiddenSize = Encoding.encodedLength * 2
                NumHiddenLayers = 9
                NumTrainingEpochs = 100
                TrainingBatchSize = 100_000
                TrainingSubBatchSize = 30_000
                DropoutRate = 0.3
                LearningRate = 1e-3
                NumIterations = 50
                NumEvaluationGames = 2000
                Device = torch.CUDA
                ModelDirPath = "./Models"
                Writer = writer
                Verbose = true
            }
        System.IO.Directory.CreateDirectory(settings.ModelDirPath)
            |> ignore

        writer.add_text(
            $"settings/NumGamesPerIteration",
            string settings.NumGamesPerIteration, 0)
        writer.add_text(
            $"settings/GameBatchSize",
            string settings.GameBatchSize, 0)
        writer.add_text(
            $"settings/SampleBranchRate",
            string settings.SampleBranchRate, 0)
        writer.add_text(
            $"settings/SampleReservoirCapacity",
            string settings.SampleReservoirCapacity, 0)
        writer.add_text(
            $"settings/HiddenSize",
            string settings.HiddenSize, 0)
        writer.add_text(
            $"settings/NumHiddenLayers",
            string settings.NumHiddenLayers, 0)
        writer.add_text(
            $"settings/NumTrainingEpochs",
            string settings.NumTrainingEpochs, 0)
        writer.add_text(
            $"settings/TrainingBatchSize",
            string settings.TrainingBatchSize, 0)
        writer.add_text(
            $"settings/TrainingSubBatchSize",
            string settings.TrainingSubBatchSize, 0)
        writer.add_text(
            $"settings/LearningRate",
            string settings.LearningRate, 0)
        writer.add_text(
            $"settings/NumIterations",
            string settings.NumIterations, 0)
        writer.add_text(
            $"settings/NumEvaluationGames",
            string settings.NumEvaluationGames, 0)

        settings
