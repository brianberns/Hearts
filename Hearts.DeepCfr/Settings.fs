namespace Hearts.DeepCfr

open System

/// Hyperparameters.
type Settings =
    {
        /// Random number generator.
        Random : Random

        /// Compensation for n-1 players on the other team.
        CutthroatCompensation : int

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

        /// Number of epochs to use when training the strategy model.
        NumStrategyTrainEpochs : int

        /// Batch size to use when training the strategy model.
        StrategyBatchSize : int

        /// Number of strategy samples to keep.
        NumStrategySamples : int

        /// Tensorboard writer.
        Writer : TorchSharp.Modules.SummaryWriter

        /// Verbose output?
        Verbose : bool
    }

[<AutoOpen>]
module Settings =

    /// Hyperparameters.
    let settings =
        let seed = 0
        TorchSharp.torch.manual_seed(seed) |> ignore
        {
            Random = Random(seed)
            CutthroatCompensation = 9
            HiddenSize = Encoding.encodedLength * 2
            LearningRate = 1e-3
            NumAdvantageTrainEpochs = 2000
            AdvantageBatchSize = 1000
            NumAdvantageSamples = 1_000_000
            StrategyBatchSize = 1000
            NumTraversals = 10
            NumIterations = 400
            NumStrategyTrainEpochs = 4000
            NumStrategySamples = 1_000_000
            Writer =
                let timespan = DateTime.Now - DateTime.Today
                TorchSharp.torch.utils.tensorboard.SummaryWriter(
                    $"runs/run%05d{int timespan.TotalSeconds}")
            Verbose = true
        }
