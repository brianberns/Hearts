namespace Hearts.Model

open TorchSharp
open type torch
open type torch.nn

open PlayingCards

/// Neural network that maps an input tensor to an output
/// tensor.
type Network = Module<Tensor, Tensor>

module Network =

    /// Size of neural network input.
    let inputSize = Encoding.encodedLength

    /// Size of a neural network hidden layer.
    let hiddenSize = Encoding.encodedLength * 6

    /// Size of neural network output.
    let outputSize = Card.numCards

type SkipConnection(inner : Network) as this =
    inherit Network($"{inner.GetName()}Skip")

    do this.register_module("inner", inner)

    override _.forward(input) =
        use x = input --> inner
        x + input

/// Model used for learning advantages.
type AdvantageModel(device : torch.Device) as this =
    inherit Network("AdvantageModel")

    let mutable curDevice = device

    let SkipConnection(inner) = new SkipConnection(inner)

    let sequential =
        Sequential(

            Linear(
                Network.inputSize,
                Network.hiddenSize,
                device = device),
            ReLU(),
            Dropout(),

            SkipConnection(
                Sequential(
                    Linear(
                        Network.hiddenSize,
                        Network.hiddenSize,
                        device = device),
                    ReLU(),
                    Dropout())),

            Linear(
                Network.hiddenSize,
                Network.outputSize,
                device = device))

    do
        this.RegisterComponents()
        this.MoveTo(device)   // make sure module itself is on the right device

    member _.MoveTo(device) =
        lock this (fun () ->
            curDevice <- device
            this.``to``(device) |> ignore)

    member _.Device = curDevice

    override _.forward(input) = 
        sequential.forward(input)

module AdvantageModel =

    /// Gets the advantage for the given info set (hand + deal).
    let getAdvantage hand deal (model : AdvantageModel) =
        use _ = torch.no_grad()
        use input =
            let encoded = Encoding.encode hand deal
            tensor(
                encoded, device = model.Device,
                dtype = ScalarType.Float32)
        input --> model
