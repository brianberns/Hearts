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

        /// Verbose output?
        Verbose : bool
    }

[<AutoOpen>]
module Settings =

    let private writer =
        let timespan = DateTime.Now - DateTime.Today
        torch.utils.tensorboard.SummaryWriter(
            $"runs/run%05d{int timespan.TotalSeconds}")

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
                HiddenSize = Encoding.encodedLength * 2
                LearningRate = 1e-3
                NumAdvantageTrainEpochs = 30_000
                AdvantageBatchSize = 10_000
                NumAdvantageSamples = Int32.MaxValue
                NumTraversals = 1000
                NumIterations = 10
                NumEvaluationDeals = 10_000
                NumStrategyTrainEpochs = 30_000
                StrategyBatchSize = 10_000
                NumStrategySamples = Int32.MaxValue
                Device = torch.CUDA
                Writer = writer
                Verbose = true
            }

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
