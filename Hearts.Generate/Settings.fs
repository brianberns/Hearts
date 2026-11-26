namespace Hearts.Generate

open System.IO
open TorchSharp

open Hearts
open Hearts.Model

/// Hyperparameters.
type Settings =
    {
        /// Rules to use for sample generation.
        Rules : Rules

        /// Total number of games to create when generating sample
        /// data at the start of each iteration.
        NumGamesPerIteration : int

        /// Number of games to create per batch when generating
        /// sample data at the start of each iteration. E.g. 800
        /// games at 20 games/batch is 40 batches.
        GameBatchSize : int

        /// Number of information sets in an inference batch.
        InferenceBatchSize : int

        /// Branch rate of the move tree when generating sample data
        /// at the start of each iteration. Larger values generate
        /// more samples.
        /// https://chatgpt.com/c/67b26aab-6504-8000-ba0e-0ae3c8a614ff
        SampleBranchRate : float

        /// Whether to generate game- or deal-level payoffs.
        GameLevelPayoff : bool

        /// Input and output size of a hidden layer within the neural
        /// network.
        HiddenSize : int

        /// Number of hidden layers within the neural network.
        NumHiddenLayers : int

        /// Device to use for running models.
        Device : torch.Device

        /// Tensorboard writer.
        Writer : Modules.SummaryWriter

        /// Path to directory where models will be saved.
        ModelDirPath : string

        /// Verbose output?
        Verbose : bool
    }

module Settings =

    /// Creates default settings.
    let create writer =
        {
            Rules = Rules.arena
#if DEBUG
            NumDealsPerIteration = 5
#else
            NumGamesPerIteration = 2_000
#endif
            GameBatchSize = 3
            InferenceBatchSize = 50_000
            SampleBranchRate = 0.23
            GameLevelPayoff = false
            HiddenSize = 1080   // inherited from original deal-level model
            NumHiddenLayers = 4
            Device = torch.CUDA
            ModelDirPath = Path.Combine(".", "Models")
            Writer = writer
            Verbose = true
        }

    /// Writes settings to Tensorboard.
    let write settings =
        let writer = settings.Writer
        writer.add_text(
            $"settings/NumGamesPerIteration",
            string settings.NumGamesPerIteration, 0)
        writer.add_text(
            $"settings/GameBatchSize",
            string settings.GameBatchSize, 0)
        writer.add_text(
            $"settings/InferenceBatchSize",
            string settings.InferenceBatchSize, 0)
        writer.add_text(
            $"settings/SampleBranchRate",
            string settings.SampleBranchRate, 0)
        writer.add_text(
            $"settings/GameLevelPayoff",
            string settings.GameLevelPayoff, 0)
        writer.add_text(
            $"settings/HiddenSize",
            string settings.HiddenSize, 0)
        writer.add_text(
            $"settings/NumHiddenLayers",
            string settings.NumHiddenLayers, 0)

        settings.ModelDirPath   // to-do: move this somewhere else?
            |> Directory.CreateDirectory
            |> ignore
