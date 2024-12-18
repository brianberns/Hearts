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

/// Neural network that maps an input tensor to an output
/// tensor.
type Network = Module<Tensor, Tensor>

/// Loss function.
type Loss = Loss<Tensor, Tensor, Tensor>

module Network =

    /// Length of neural network input.
    let inputSize = Encoding.encodedLength

    /// Length of neural network output.
    let outputSize = Card.allCards.Length

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
        assert(regrets.Count = Network.outputSize)
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
type AdvantageModel =
    {
        /// Neural network.
        Network : Network

        /// Training optimizer.
        Optimizer : Optimizer

        /// Training loss function.
        Loss : Loss
    }

    member this.Dispose() =
        this.Network.Dispose()
        this.Optimizer.Dispose()
        this.Loss.Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()

module AdvantageModel =

    /// Creates an advantage model.
    let create hiddenSize learningRate =
        let network =
            Sequential(
                Linear(
                    Network.inputSize,
                    hiddenSize,
                    device = settings.Device),
                ReLU(),
                Linear(
                    hiddenSize,
                    Network.outputSize,
                    device = settings.Device))
        {
            Network = network
            Optimizer =
                Adam(
                    network.parameters(),
                    lr = learningRate)
            Loss = MSELoss()
        }

    /// Gets the advantage for the given info set (hand + deal).
    let getAdvantage hand deal model =
        let encoded = Encoding.encode hand deal
        tensor(encoded, device = settings.Device)
            --> model.Network

    /// Trains the given model using the given samples.
    let train samples model =

            // prepare training data
        let tensors =
            samples
                |> Seq.toArray
                |> Array.randomShuffle
                |> Array.chunkBySize settings.AdvantageBatchSize
                |> Array.map (fun batch ->
                    let inputs, targets, iters =
                        batch
                            |> Array.map (fun sample ->
                                let input =
                                    Encoding.encode sample.Hand sample.Deal
                                let target = sample.Regrets
                                let iter =
                                    (sample.Iteration + 1)   // make 1-based
                                        |> float32
                                        |> sqrt
                                        |> Seq.singleton
                                input, target, iter)
                            |> Array.unzip3
                    Tensor.ofSeq inputs,
                    Tensor.ofSeq targets,
                    Tensor.ofSeq iters)

        [|
            for _ = 1 to settings.NumAdvantageTrainEpochs do
                Array.last [|
                    for inputs, targets, iters in tensors do

                            // forward pass
                        use loss =
                            use outputs = inputs --> model.Network
                            use outputs' = iters * outputs   // favor later iterations
                            use targets' = iters * targets
                            model.Loss.forward(outputs', targets')

                            // backward pass and optimize
                        model.Optimizer.zero_grad()
                        loss.backward()
                        use _ = model.Optimizer.step()

                        loss.item<float32>()
                |]
        |]

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
        assert(strategy.Count = Network.outputSize)
        {
            Hand = hand
            Deal = deal
            Strategy = strategy
            Iteration = iteration
        }

/// Model used for learning strategy.
type StrategyModel =
    {
        /// Neural network.
        Network : Network

        /// Training optimizer.
        Optimizer : Optimizer

        /// Training loss function.
        Loss : Loss

        /// Softmax layer.
        Softmax : Softmax
    }

    member this.Dispose() =
        this.Network.Dispose()
        this.Optimizer.Dispose()
        this.Loss.Dispose()
        this.Softmax.Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()

module StrategyModel =

    /// Creates a strategy model.
    let create hiddenSize learningRate =
        let network =
            Sequential(
                Linear(
                    Network.inputSize,
                    hiddenSize,
                    device = settings.Device),
                ReLU(),
                Linear(
                    hiddenSize,
                    Network.outputSize,
                    device = settings.Device))
        {
            Network = network
            Optimizer =
                Adam(
                    network.parameters(),
                    lr = learningRate)
            Loss = MSELoss()
            Softmax = Softmax(dim = -1)
        }

    /// Trains the given model using the given samples.
    let train samples model =

            // prepare training data
        let tensors =
            samples
                |> Seq.toArray
                |> Array.randomShuffle
                |> Array.chunkBySize settings.StrategyBatchSize
                |> Array.map (fun batch ->
                    let inputs, targets, iters =
                        batch
                            |> Array.map (fun sample ->
                                let input =
                                    Encoding.encode sample.Hand sample.Deal
                                let target = sample.Strategy
                                let iter =
                                    (sample.Iteration + 1)   // make 1-based
                                        |> float32
                                        |> sqrt
                                        |> Seq.singleton
                                input, target, iter)
                            |> Array.unzip3
                    Tensor.ofSeq inputs,
                    Tensor.ofSeq targets,
                    Tensor.ofSeq iters)

        [|
            for _ = 1 to settings.NumStrategyTrainEpochs do
                Array.last [|
                    for inputs, targets, iters in tensors do

                            // forward pass
                        use loss =
                            use outputs =
                                (inputs --> model.Network)
                                    |> model.Softmax.forward
                            use outputs' = iters * outputs   // favor later iterations
                            use targets' = iters * targets
                            model.Loss.forward(outputs', targets')

                            // backward pass and optimize
                        model.Optimizer.zero_grad()
                        loss.backward()
                        use _ = model.Optimizer.step()

                        loss.item<float32>()
                |]
        |]

    /// Gets the strategy for the given info set (hand + deal).
    let getStrategy hand deal model =
        let encoded = Encoding.encode hand deal
        tensor(encoded, device = settings.Device)
            --> model.Network
            |> model.Softmax.forward
