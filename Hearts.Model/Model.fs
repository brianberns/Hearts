namespace Hearts.Model

open TorchSharp
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

module Network =

    /// Length of neural network input.
    let inputSize = Encoding.encodedLength

    /// Length of neural network output.
    let outputSize = Card.numCards

/// An observed advantage event.
type AdvantageSample =
    {
        /// Current player's hand.
        Hand : Hand

        /// Current deal.
        Deal : ClosedDeal

        /// Observed regrets.
        Regrets : Vector<float32>
    }

module AdvantageSample =

    /// Creates an advantage sample.
    let create hand deal (regrets : Vector<_>) =
        assert(regrets.Count = Network.outputSize)
        {
            Hand = hand
            Deal = deal
            Regrets = regrets
        }

module Tensor =
     
    /// Converts the given rows to a tensor.
    let ofSeq (rows : seq<#seq<float32>>) =
        tensor(array2D rows, device = settings.Device)

/// Model used for learning advantages.
type AdvantageModel() as this =
    inherit Network("AdvantageModel")

    let sequential =
        Sequential(
            Linear(
                Network.inputSize,
                settings.HiddenSize,
                device = settings.Device),
            ReLU(),
            Linear(
                settings.HiddenSize,
                Network.outputSize,
                device = settings.Device))

    do this.RegisterComponents()

    override _.forward(input) = 
        sequential.forward(input)

module AdvantageModel =

    /// Gets the advantage for the given info set (hand + deal).
    let getAdvantage hand deal model =
        use _ = torch.no_grad()
        let encoded = Encoding.encode hand deal
        tensor(encoded, device = settings.Device)
            --> model

    /// Trains the given model using the given samples.
    let train samples (model : AdvantageModel) =

            // prepare training data
        let tensors =
            samples
                |> Array.randomShuffle
                |> Array.chunkBySize settings.AdvantageBatchSize
                |> Array.map (fun batch ->
                    let inputs, targets =
                        batch
                            |> Array.map (fun sample ->
                                let input =
                                    Encoding.encode sample.Hand sample.Deal
                                let target = sample.Regrets
                                input, target)
                            |> Array.unzip
                    Tensor.ofSeq inputs,
                    Tensor.ofSeq targets)

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
                        for inputs, targets in tensors do

                                // forward pass
                            use loss =
                                use outputs = inputs --> model
                                loss.forward(outputs, targets)

                                // backward pass and optimize
                            optimizer.zero_grad()
                            loss.backward()
                            use _ = optimizer.step()

                            loss.item<float32>()
                    |]
            |]
        model.eval()
        losses
