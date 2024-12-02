namespace Hearts.DeepCfr

open System

[<AutoOpen>]
module Settings =

    /// Hyperparameters.
    let settings =
        let seed = 0
        TorchSharp.torch.manual_seed(seed) |> ignore
        {|
            /// Random number generator.
            Random = Random(seed)

            /// Size of a neural network hidden layer.
            HiddenSize = Encoding.encodedLength * 2

            /// Optimizer learning rate.
            LearningRate = 1e-3

            /// Number of steps to use when training advantage models.
            NumAdvantageTrainSteps = 2000

            /// Number of advantage samples to keep.
            NumAdvantageSamples = 1_000_000

            /// Number of deals to traverse during each iteration.
            NumTraversals = 10

            /// Number of iterations to perform.
            NumIterations = 400

            /// Number of steps to use when training the strategy model.
            NumStrategyTrainSteps = 4000

            /// Number of strategy samples to keep.
            NumStrategySamples = 1_000_000

            /// Tensorboard writer.
            Writer =
                let timespan = DateTime.Now - DateTime.Today
                TorchSharp.torch.utils.tensorboard.SummaryWriter(
                    $"runs/run%05d{int timespan.TotalSeconds}")

            /// Verbose output?
            Verbose = true
        |}
