namespace Hearts.DeepCfr

open System

open TorchSharp
open TorchSharp.Modules
open type torch
open type torch.nn
open type torch.optim
open FSharp.Core.Operators   // reclaim "float32" and other F# operators

open MathNet.Numerics.LinearAlgebra

open PlayingCards
open Hearts

/// Loss function.
type Loss = Loss<Tensor, Tensor, Tensor>

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

module Tensor =
     
    /// Converts the given rows to a tensor.
    let ofSeq (rows : seq<#seq<float32>>) =
        tensor(array2D rows, device = settings.Device)

/// Model used for learning advantages.
type AdvantageModel() as this =
    inherit Module<Encoding, Tensor>("AdvantageModel")

    let playerOutputSize = 2 * int Seat.numSeats
    let playerBranch =
        Embedding(
            Seat.numSeats,
            playerOutputSize,
            device = settings.Device)

    let cardInputSize = Card.numCards + 1   // Card.numCards index -> missing card
    let handOutputSize = settings.HiddenSize
    let handBranch =
        Embedding(
            cardInputSize,
            handOutputSize,
            padding_idx = Card.numCards,   // missing card -> zero vector
            device = settings.Device)

    let otherUnplayedOutputSize = settings.HiddenSize
    let otherUnplayedBranch =
        Embedding(
            cardInputSize,
            otherUnplayedOutputSize,
            padding_idx = Card.numCards,   // missing card -> zero vector
            device = settings.Device)

    let combinedInputSize =
        playerOutputSize
            + handOutputSize
            + otherUnplayedOutputSize
    let combined =
        Sequential(
            ReLU(),
            Linear(
                combinedInputSize,
                settings.HiddenSize,
                device = settings.Device),
            ReLU(),
            Linear(
                settings.HiddenSize,
                Card.numCards,
                device = settings.Device))

    do this.RegisterComponents()

    override _.forward(encoding) =

        use _ = torch.NewDisposeScope()

        let playerOutput =
            (encoding.Player --> playerBranch)
                .sum(dim = 1)   // sum along sequence dimension

        let handOutput =
            (encoding.Hand --> handBranch)
                .sum(dim = 1)   // sum along sequence dimension

        let otherUnplayedOutput =
            (encoding.OtherUnplayed --> otherUnplayedBranch)
                .sum(dim = 1)   // sum along sequence dimension

        let combinedInput =
            torch.cat(
                [|
                    playerOutput
                    handOutput
                    otherUnplayedOutput
                |],
                dim = 1)
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
            2L * int64 Seat.numSeats,
            device = settings.Device)

    let combined =
        Sequential(
            ReLU(),
            Linear(
                2L * int64 Seat.numSeats,
                Card.numCards,
                device = settings.Device))

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
