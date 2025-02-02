namespace Hearts.DeepCfr

open TorchSharp
open type torch
open type torch.nn
open type torch.optim
open FSharp.Core.Operators   // reclaim "float32" and other F# operators

open MathNet.Numerics.LinearAlgebra

open PlayingCards
open Hearts

type Module = Module<Tensor, Tensor>

module Tensor =
     
    /// Converts the given rows to a tensor.
    let ofSeq (rows : seq<#seq<float32>>) =
        tensor(array2D rows)

type SkipConnection(inner : Module) as this =
    inherit Module($"{inner.GetName()}Skip")

    do this.register_module("inner", inner)

    override _.forward(input) =
        (input --> inner) + input

// https://copilot.microsoft.com/chats/aDA4xBmW2ssysdLZnZqyN
type OneHot(length : int) =
    inherit Module("OneHot")
    // Pre-cache the one-hot vectors including the padding vector at the end.
    let oneHotVectors =
        // Create identity matrix for one-hot vectors.
        let eye = torch.eye(length, device = torch.CPU)
        // Create padding vector of zeros.
        let padding = torch.zeros(1L, length, device = torch.CPU)
        // Concatenate to form the lookup tensor.
        torch.cat([| eye; padding |], dim=0)

    override this.forward(input: Tensor) =
        // Replace indices equal to 'length' with 'length' (padding index).
        let adjustedIndices =
            input.where(input.eq(torch.tensor(length, device = torch.CPU)), torch.full_like(input, length))
        // Flatten indices for batch indexing.
        let flatIndices = adjustedIndices.flatten()
        // Gather one-hot vectors using the flattened indices.
        let gathered = oneHotVectors.index_select(0L, flatIndices)
        // Reshape back to the original input shape plus the one-hot dimension.
        gathered.view([| yield! adjustedIndices.shape; length |])

type Branch =
    {
        Model : Module
        OutputSize : int
    }

module Branch =

    let create model outputSize =
        {
            Model = model
            OutputSize = outputSize
        }

// https://github.com/dotnet/TorchSharp/issues/1438
[<AutoOpen>]
type Modules =

    static member Embedding(nIn, nOut, ?padding_idx) =
        torch.nn.Embedding(
            nIn, nOut,
            ?padding_idx = padding_idx,
            device = torch.get_default_device())

    static member Linear(nIn, nOut) =
        torch.nn.Linear(
            nIn, nOut,
            device = torch.get_default_device())

    static member SkipConnection(inner) =
        new SkipConnection(inner)

    static member OneHot(nDim) =
        new OneHot(nDim)

/// An observed advantage event.
type AdvantageSample =
    {
        /// Current player's hand.
        Hand : Hand

        /// Current deal.
        Deal : ClosedDeal

        /// Observed regrets.
        Regrets : Vector<float32>

        /// 0-based iteration number.
        Iteration : int
    }

module AdvantageSample =

    /// Creates an advantage sample.
    let create hand deal (regrets : Vector<_>) iteration =
        assert(regrets.Count = Card.numCards)
        {
            Hand = hand
            Deal = deal
            Regrets = regrets
            Iteration = iteration
        }

/// Model used for learning advantages.
type AdvantageModel() as this =
    inherit Module<Encoding, Tensor>("AdvantageModel")

    let cardBranch =
        let nDim = Card.numCards
        let model = OneHot(nDim)
        Branch.create model nDim

    let playerBranch =
        let nDim = Seat.numSeats
        let model = OneHot(nDim)
        Branch.create model nDim

    let handBranch = cardBranch

    let otherUnplayedBranch = cardBranch

    let trickBranch = cardBranch

    let voidsBranch =
        let nDim = Encoding.voidsLength
        let model = OneHot(nDim)
        Branch.create model nDim

    let combinedInputSize =
        Seat.numSeats                                 // current player
            + Card.numCards                           // current player's hand
            + Card.numCards                           // other unplayed cards
            + ((Seat.numSeats - 1) * Card.numCards)   // current trick
            + (Suit.numSuits * Seat.numSeats)         // voids
            + Seat.numSeats                           // score
    let combined =
        Sequential(
            Linear(
                combinedInputSize,
                settings.HiddenSize),
            ReLU(),
            Linear(
                settings.HiddenSize,
                Card.numCards))

    do this.RegisterComponents()

    override _.forward(encoding) =

        use _ = torch.NewDisposeScope()

        let playerOutput =
            (encoding.Player
                --> playerBranch.Model)
                .squeeze(dim = 1)   // exactly one current player

            // [B, 6] -> [B, 6, embedding] -> [B, sum of embeddings]
        let handOutput =
            (encoding.Hand
                --> handBranch.Model)
                .sum(dim = 1)   // sum of unordered card vectors

        let otherUnplayedOutput =
            (encoding.OtherUnplayed
                --> otherUnplayedBranch.Model)
                .sum(dim = 1)   // sum of unordered card vectors

            // [B, 3] -> [B, 3, embedding] -> [B, 3 concatenated embeddings]
        let batchSize = encoding.Trick.shape[0]
        let trickOutput =
            (encoding.Trick
                --> trickBranch.Model)
                .view(batchSize, -1)   // concatenate card vectors in order
        assert(
            trickOutput.shape =
                [|
                    batchSize
                    int64 (
                        Encoding.trickLength
                            * trickBranch.OutputSize)
                |])

        let voidsOutput =
            (encoding.Voids
                --> voidsBranch.Model)
                .sum(dim = 1)   // sum of unordered (seat, suit) vectors

        let combinedInput =
            torch.cat(
                [|
                    playerOutput
                    handOutput
                    otherUnplayedOutput
                    trickOutput
                    voidsOutput
                    encoding.Score.float()
                |],
                dim = 1).``to``(torch.CUDA)
        let result =
            combinedInput --> combined

        result.MoveToOuterDisposeScope()

module AdvantageModel =

    /// Gets the advantage for the given info set (hand + deal).
    let getAdvantage hand deal (model : AdvantageModel) =
        use _ = torch.no_grad()
        Encoding.encode hand deal
            |> model.forward

    /// Trains the given model using the given samples.
    let train samples (model : AdvantageModel) =

            // prepare training data
        let tensors =
            samples
                |> Seq.toArray
                |> Array.randomShuffle
                |> Array.chunkBySize settings.AdvantageBatchSize
                |> Array.map (fun batch ->
                    let encodings, targets, iters =
                        batch
                            |> Array.map (fun sample ->
                                let encoding =
                                    Encoding.encode sample.Hand sample.Deal
                                let target = sample.Regrets
                                let iter =
                                    (sample.Iteration + 1)   // make 1-based
                                        |> float32
                                        |> sqrt
                                        |> Seq.singleton
                                encoding, target, iter)
                            |> Array.unzip3
                    Encoding.concat encodings,
                    Tensor.ofSeq targets,
                    Tensor.ofSeq iters)

        use optimizer =
            Adam(
                model.parameters(),
                settings.LearningRate)
        use loss = MSELoss()
        model.train()
        let losses =
            [|
                for _ = 1 to settings.NumAdvantageTrainEpochs do
                    Array.last [|
                        for encoding, targets, iters in tensors do

                                // forward pass
                            use loss =
                                use outputs = model.forward(encoding)
                                use outputs' = iters * outputs   // favor later iterations
                                use targets' = iters * targets
                                loss.forward(outputs', targets')

                                // backward pass and optimize
                            optimizer.zero_grad()
                            loss.backward()
                            use _ = optimizer.step()

                            loss.item<float32>()
                    |]
            |]
        model.eval()
        losses

/// An observed strategy event.
type StrategySample =
    {
        /// Current player's hand.
        Hand : Hand

        /// Current deal.
        Deal : ClosedDeal

        /// Observed strategy.
        Strategy : Vector<float32>

        /// O-based iteration number.
        Iteration : int
    }

module StrategySample =

    /// Creates a strategy sample.
    let create hand deal (strategy : Vector<_>) iteration =
        assert(strategy.Count = Card.numCards)
        {
            Hand = hand
            Deal = deal
            Strategy = strategy
            Iteration = iteration
        }

/// Model used for learning strategy.
type StrategyModel() as this =
    inherit Module<Encoding, Tensor>("StrategyModel")

    let playerBranch =
        Embedding(
            Seat.numSeats,
            2L * int64 Seat.numSeats)

    let combined =
        Sequential(
            ReLU(),
            Linear(
                2L * int64 Seat.numSeats,
                Card.numCards))

    let softmax = Softmax(dim = -1)

    do this.RegisterComponents()

    member _.Softmax = softmax

    override _.forward(encoding) = 

        use _ = torch.NewDisposeScope()

        let playerOutput =
            (encoding.Player --> playerBranch)
                .sum(dim = 1)   // sum along sequence dimension

        let result =
            playerOutput --> combined

        result.MoveToOuterDisposeScope()

module StrategyModel =

    /// Trains the given model using the given samples.
    let train samples (model : StrategyModel) =

            // prepare training data
        let tensors =
            samples
                |> Seq.toArray
                |> Array.randomShuffle
                |> Array.chunkBySize settings.StrategyBatchSize
                |> Array.map (fun batch ->
                    let encodings, targets, iters =
                        batch
                            |> Array.map (fun sample ->
                                let encoding =
                                    Encoding.encode sample.Hand sample.Deal
                                let target = sample.Strategy
                                let iter =
                                    (sample.Iteration + 1)   // make 1-based
                                        |> float32
                                        |> sqrt
                                        |> Seq.singleton
                                encoding, target, iter)
                            |> Array.unzip3
                    Encoding.concat encodings,
                    Tensor.ofSeq targets,
                    Tensor.ofSeq iters)

        use optimizer =
            Adam(
                model.parameters(),
                settings.LearningRate)
        use loss = MSELoss()
        model.train()
        let losses =
            [|
                for _ = 1 to settings.NumStrategyTrainEpochs do
                    Array.last [|
                        for encoding, targets, iters in tensors do

                                // forward pass
                            use loss =
                                use outputs =
                                    use temp = model.forward(encoding)
                                    model.Softmax.forward(temp)
                                use outputs' = iters * outputs   // favor later iterations
                                use targets' = iters * targets
                                loss.forward(outputs', targets')

                                // backward pass and optimize
                            optimizer.zero_grad()
                            loss.backward()
                            use _ = optimizer.step()

                            loss.item<float32>()
                    |]
            |]
        model.eval()
        losses

    /// Gets the strategy for the given info set (hand + deal).
    let getStrategy hand deal (model : StrategyModel) =
        use _ = torch.no_grad()
        Encoding.encode hand deal
            |> model.forward
            |> model.Softmax.forward
