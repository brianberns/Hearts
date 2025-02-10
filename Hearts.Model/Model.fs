namespace Hearts.Model

open TorchSharp
open type torch
open type torch.nn

open PlayingCards

/// Neural network that maps an input tensor to an output
/// tensor.
type Network = Module<Tensor, Tensor>

module Network =

    /// Length of neural network input.
    let inputSize = Encoding.encodedLength

    /// Length of neural network output.
    let outputSize = Card.numCards

/// Model used for learning advantages.
type AdvantageModel() as this =
    inherit Network("AdvantageModel")

    let sequential =
        Sequential(
            Linear(
                Network.inputSize,
                Settings.hiddenSize,
                device = Settings.device),
            ReLU(),
            Linear(
                Settings.hiddenSize,
                Network.outputSize,
                device = Settings.device))

    do this.RegisterComponents()

    override _.forward(input) = 
        sequential.forward(input)

module AdvantageModel =

    /// Gets the advantage for the given info set (hand + deal).
    let getAdvantage hand deal model =
        use _ = torch.no_grad()
        let encoded = Encoding.encode hand deal
        tensor(encoded, device = Settings.device)
            --> model
