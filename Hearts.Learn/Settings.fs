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

        /// Size of a neural network hidden layer.
        HiddenSize : int

        /// Number of hidden layers.
        NumHiddenLayers : int

        /// Optimizer learning rate.
        LearningRate : float

        /// Number of epochs to use when training advantage models.
        NumAdvantageTrainEpochs : int

        /// Batch size to use when training advantage models.
        AdvantageBatchSize : int

        /// Sub-batch size to use when training advantage models.
        AdvantageSubBatchSize : int

        /// Number of advantage samples to keep.
        NumAdvantageSamples : int

        /// Number of iterations to perform.
        NumIterations : int

        /// Number of deals to evaluate model.
        NumEvaluationDeals : int

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
                NumDealsPerIteration = 8000
                DealBatchSize = 200
                SampleBranchRate = 1.5
                HiddenSize = Encoding.encodedLength * 6
                NumHiddenLayers = 2
                LearningRate = 1e-3
                NumAdvantageTrainEpochs = 500
                AdvantageBatchSize = 1_000_000
                AdvantageSubBatchSize = 80_000
                NumAdvantageSamples = 100_000_000
                NumIterations = 50
                NumEvaluationDeals = 20000
                Device = torch.CUDA
                ModelDirPath = "./Models"
                Writer = writer
                Verbose = true
            }
        System.IO.Directory.CreateDirectory(settings.ModelDirPath)
            |> ignore

        writer.add_text(
            $"settings/HiddenSize",
            string settings.HiddenSize, 0)
        writer.add_text(
            $"settings/NumHiddenLayers",
            string settings.NumHiddenLayers, 0)
        writer.add_text(
            $"settings/LearningRate",
            string settings.LearningRate, 0)
        writer.add_text(
            $"settings/SampleDecay",
            string settings.SampleBranchRate, 0)
        writer.add_text(
            $"settings/NumAdvantageTrainEpochs",
            string settings.NumAdvantageTrainEpochs, 0)
        writer.add_text(
            $"settings/NumAdvantageSamples",
            string settings.NumAdvantageSamples, 0)
        writer.add_text(
            $"settings/AdvantageBatchSize",
            string settings.AdvantageBatchSize, 0)
        writer.add_text(
            $"settings/AdvantageSubBatchSize",
            string settings.AdvantageSubBatchSize, 0)
        writer.add_text(
            $"settings/NumTraversals",
            string settings.NumDealsPerIteration, 0)
        writer.add_text(
            $"settings/TraversalBatchSize",
            string settings.DealBatchSize, 0)
        writer.add_text(
            $"settings/NumIterations",
            string settings.NumIterations, 0)
        writer.add_text(
            $"settings/NumEvaluationDeals",
            string settings.NumEvaluationDeals, 0)

        settings
