namespace Hearts.Learn

open System
open TorchSharp
open Hearts.Model

/// Hyperparameters.
type Settings =
    {
        /// Size of an exchange model hidden layer.
        ExchangeHiddenSize : int

        /// Size of a playout model hidden layer.
        PlayoutHiddenSize : int

        /// Number of hidden layers.
        NumHiddenLayers : int

        /// Optimizer learning rate.
        LearningRate : float

        /// Sample decay control. Greater values cause greater
        /// branching.
        /// https://chatgpt.com/c/67b26aab-6504-8000-ba0e-0ae3c8a614ff
        SampleDecay : float

        /// Number of epochs to use when training advantage models.
        NumAdvantageTrainEpochs : int

        /// Batch size to use when training advantage models.
        AdvantageBatchSize : int

        /// Sub-batch size to use when training advantage models.
        AdvantageSubBatchSize : int

        /// Number of advantage samples to keep.
        NumAdvantageSamples : int

        /// Number of deals to traverse during each iteration.
        NumTraversals : int

        /// Number of deals to traverse in each batch.
        TraversalBatchSize : int

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
                ExchangeHiddenSize =
                    Encoding.Exchange.encodedLength * 2
                PlayoutHiddenSize =
                    Encoding.Playout.encodedLength * 2
                NumHiddenLayers = 1
                LearningRate = 1e-3
                SampleDecay = 0.17
                NumAdvantageTrainEpochs = 500
                AdvantageBatchSize = 1_000_000
                AdvantageSubBatchSize = 80_000
                NumAdvantageSamples = 100_000_000
                NumTraversals = 8000
                TraversalBatchSize = 200
                NumIterations = 25
                NumEvaluationDeals = 20000
                Device = torch.CUDA
                ModelDirPath = "./Models"
                Writer = writer
                Verbose = true
            }
        System.IO.Directory.CreateDirectory(settings.ModelDirPath)
            |> ignore

        writer.add_text(
            $"settings/ExchangeHiddenSize",
            string settings.ExchangeHiddenSize, 0)
        writer.add_text(
            $"settings/PlayoutHiddenSize",
            string settings.PlayoutHiddenSize, 0)
        writer.add_text(
            $"settings/NumHiddenLayers",
            string settings.NumHiddenLayers, 0)
        writer.add_text(
            $"settings/LearningRate",
            string settings.LearningRate, 0)
        writer.add_text(
            $"settings/SampleDecay",
            string settings.SampleDecay, 0)
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
            string settings.NumTraversals, 0)
        writer.add_text(
            $"settings/TraversalBatchSize",
            string settings.TraversalBatchSize, 0)
        writer.add_text(
            $"settings/NumIterations",
            string settings.NumIterations, 0)
        writer.add_text(
            $"settings/NumEvaluationDeals",
            string settings.NumEvaluationDeals, 0)

        settings
