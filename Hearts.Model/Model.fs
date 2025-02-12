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
    let hiddenSize = Encoding.encodedLength

    /// Size of neural network output.
    let outputSize = Card.numCards

/// Model used for learning advantages.
type AdvantageModel(device : torch.Device) as this =
    inherit Network("AdvantageModel")

    let sequential =
        Sequential(
            Linear(
                Network.inputSize,
                Network.hiddenSize,
                device = device),
            ReLU(),
            Linear(
                Network.hiddenSize,
                Network.outputSize,
                device = device))

    do
        this.RegisterComponents()
        this.``to``(device) |> ignore   // make sure module itself is on the right device

    member _.Device = device

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
