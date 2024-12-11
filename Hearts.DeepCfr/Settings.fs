﻿namespace Hearts.DeepCfr

open System
open TorchSharp

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

    /// Hyperparameters.
    let settings =
        let seed = 0
        torch.manual_seed(seed) |> ignore
        torch.cuda.manual_seed_all(seed)
        {
            Random = Random(seed)
            CutthroatCompensation = 9
            HiddenSize = Encoding.encodedLength * 2
            LearningRate = 1e-3
            NumAdvantageTrainEpochs = 3_000
            AdvantageBatchSize = 10_000
            NumAdvantageSamples = 1_000_000
            NumTraversals = 10
            NumIterations = 100
            NumEvaluationDeals = 10000
            NumStrategyTrainEpochs = 4_000
            StrategyBatchSize = 10_000
            NumStrategySamples = 1_000_000
            Device = torch.CUDA
            Writer =
                let timespan = DateTime.Now - DateTime.Today
                torch.utils.tensorboard.SummaryWriter(
                    $"runs/run%05d{int timespan.TotalSeconds}")
            Verbose = true
        }
