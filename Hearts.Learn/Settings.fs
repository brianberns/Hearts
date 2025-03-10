namespace Hearts.Learn

open System
open TorchSharp
open Hearts.Model

/// Hyperparameters.
type Settings =
    {
        /// Random number generator.
        Random : Random

        /// Size of a neural network hidden layer.
        HiddenSize : int

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

    /// RNG seed.
    let seed = 0

    /// Hyperparameters.
    let settings =

        torch.manual_seed(seed) |> ignore
        torch.cuda.manual_seed_all(seed)
        writer.add_text(
            $"settings/seed", string seed, 0)

        let settings =
            {
                Random = Random(seed)
                HiddenSize = Encoding.encodedLength * 4
                LearningRate = 2e-3
                SampleDecay = 0.15
                NumAdvantageTrainEpochs = 500
                AdvantageBatchSize = 1_000_000
                AdvantageSubBatchSize = 50_000
                NumAdvantageSamples = 20_000_000
                NumTraversals = 16000
                NumIterations = 25
                NumEvaluationDeals = 10000
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
            $"settings/NumIterations",
            string settings.NumIterations, 0)
        writer.add_text(
            $"settings/NumEvaluationDeals",
            string settings.NumEvaluationDeals, 0)

        settings
