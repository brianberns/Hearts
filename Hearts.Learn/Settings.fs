namespace Hearts.Learn

open System
open TorchSharp
open Hearts.Model

/// Hyperparameters.
type Settings =
    {
        /// Total number of deals to create when generating sample
        /// data at the start of each iteration.
        NumDealsPerIteration : int

        /// Number of deals to create per batch when generating
        /// sample data at the start of each iteration. E.g. 8000
        /// deals at 200 deals/batch is 40 batches.
        DealBatchSize : int

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

        /// Number of epochs per evaluation.
        NumEpochsPerEvaluation : int

        /// Batch size to use when training the model.
        TrainingBatchSize : int

        /// Sub-batch size to use when training the model.
        TrainingSubBatchSize : int

        /// Dropout rate to use when training the model.
        DropoutRate : float

        /// Optimizer learning rate to use when training the model.
        LearningRate : float

        /// Number of deals to evaluate model after training.
        NumEvaluationDeals : int

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

module TensorBoard =

    /// TensorBoard log writer.
    let createWriter () =
        let timespan = DateTime.Now - DateTime.Today
        torch.utils.tensorboard.SummaryWriter(
            $"runs/run%05d{int timespan.TotalSeconds}")

module Settings =

    /// Creates default settings.
    let create writer =
        {
            NumDealsPerIteration = 2000
            DealBatchSize = 100
            SampleBranchRate = 0.2
            SampleReservoirCapacity = 80_000_000
            HiddenSize = Encoding.encodedLength * 2
            NumHiddenLayers = 9
            NumTrainingEpochs = 100
            NumEpochsPerEvaluation = 5
            TrainingBatchSize = 200_000
            TrainingSubBatchSize = 50_000
            DropoutRate = 0.2
            LearningRate = 1e-3
            NumIterations = 50
            NumEvaluationDeals = 2000
            Device = torch.CUDA
            ModelDirPath = "./Models"
            Writer = writer
            Verbose = true
        }

    /// Writes settings to Tensorboard.
    let write settings =
        let writer = settings.Writer
        writer.add_text(
            $"settings/NumDealsPerIteration",
            string settings.NumDealsPerIteration, 0)
        writer.add_text(
            $"settings/DealBatchSize",
            string settings.DealBatchSize, 0)
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
            $"settings/NumEpochsPerEvaluation",
            string settings.NumEpochsPerEvaluation, 0)
        writer.add_text(
            $"settings/TrainingBatchSize",
            string settings.TrainingBatchSize, 0)
        writer.add_text(
            $"settings/TrainingSubBatchSize",
            string settings.TrainingSubBatchSize, 0)
        writer.add_text(
            $"settings/DropoutRate",
            string settings.DropoutRate, 0)
        writer.add_text(
            $"settings/LearningRate",
            string settings.LearningRate, 0)
        writer.add_text(
            $"settings/NumIterations",
            string settings.NumIterations, 0)
        writer.add_text(
            $"settings/NumEvaluationDeals",
            string settings.NumEvaluationDeals, 0)
