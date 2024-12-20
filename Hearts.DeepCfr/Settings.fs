namespace Hearts.DeepCfr

open System
open TorchSharp

/// Hyperparameters.
type Settings =
    {
        /// Random number generator.
        Random : Random

        /// Compensation factor for n-1 players on the other team.
        ZeroSumCompensation : int

        /// Size of a neural network hidden layer.
        HiddenSize : int

        /// Optimizer learning rate.
        LearningRate : float

        /// Number of epochs to use when training advantage models.
        NumAdvantageTrainEpochs : int

        /// Batch size to use when training advantage models.
        AdvantageBatchSize : int

        /// Number of advantage samples to keep.
        NumAdvantageSamples : int

        /// Number of deals to traverse during each iteration.
        NumTraversals : int

        /// Number of iterations to perform.
        NumIterations : int

        /// Number of deals to evaluate model.
        NumEvaluationDeals : int

        /// Number of epochs to use when training the strategy model.
        NumStrategyTrainEpochs : int

        /// Batch size to use when training the strategy model.
        StrategyBatchSize : int

        /// Number of strategy samples to keep.
        NumStrategySamples : int

        /// Device to use for training models.
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
    let private seed = 0

    /// Hyperparameters.
    let settings =

        torch.manual_seed(seed) |> ignore
        torch.cuda.manual_seed_all(seed)
        writer.add_text(
            $"settings/seed", string seed, 0)

        let settings =
            {
                Random = Random(seed)
                ZeroSumCompensation = 9
                HiddenSize = Encoding.encodedLength * 8
                LearningRate = 2e-3
                NumAdvantageTrainEpochs = 3_000
                AdvantageBatchSize = 10_000
                NumAdvantageSamples = 1_000_000
                NumTraversals = 1_000
                NumIterations = 25
                NumEvaluationDeals = 10_000
                NumStrategyTrainEpochs = 5_000
                StrategyBatchSize = 10_000
                NumStrategySamples = 1_000_000
                Device = torch.CUDA
                ModelDirPath = "./Models"
                Writer = writer
                Verbose = true
            }
        System.IO.Directory.CreateDirectory(settings.ModelDirPath)
            |> ignore

        writer.add_text(
            $"settings/ZeroSumCompensation",
            string settings.ZeroSumCompensation, 0)
        writer.add_text(
            $"settings/HiddenSize",
            string settings.HiddenSize, 0)
        writer.add_text(
            $"settings/LearningRate",
            string settings.LearningRate, 0)
        writer.add_text(
            $"settings/NumAdvantageTrainEpochs",
            string settings.NumAdvantageTrainEpochs, 0)
        writer.add_text(
            $"settings/AdvantageBatchSize",
            string settings.AdvantageBatchSize, 0)
        writer.add_text(
            $"settings/NumAdvantageSamples",
            string settings.NumAdvantageSamples, 0)
        writer.add_text(
            $"settings/NumTraversals",
            string settings.NumTraversals, 0)
        writer.add_text(
            $"settings/NumIterations",
            string settings.NumIterations, 0)
        writer.add_text(
            $"settings/NumEvaluationDeals",
            string settings.NumEvaluationDeals, 0)
        writer.add_text(
            $"settings/NumStrategyTrainEpochs",
            string settings.NumStrategyTrainEpochs, 0)
        writer.add_text(
            $"settings/StrategyBatchSize",
            string settings.StrategyBatchSize, 0)
        writer.add_text(
            $"settings/NumStrategySamples",
            string settings.NumStrategySamples, 0)

        settings
