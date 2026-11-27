namespace Hearts.Model

open System.Collections.Generic

open TorchSharp
open type torch
open type torch.nn

open PlayingCards

/// Neural network that maps an input tensor to an output
/// tensor.
type Model = Module<Tensor, Tensor>

module Model =

    /// Size of neural network input.
    let inputSize = Encoding.encodedLength

    /// Size of neural network output.
    let outputSize = Card.numCards

/// Skip connection module.
type SkipConnection(inner : Model) as this =
    inherit Model($"{inner.GetName()}Skip")

    do this.register_module("inner", inner)

    override _.forward(input) =
        use x = input --> inner
        x + input

/// Model used for learning advantages.
type AdvantageModel(
    isGameLevel : bool,
    hiddenSize : int,
    numHiddenLayers : int,
    dropoutRate : float,
    device : torch.Device) as this =
    inherit Model("AdvantageModel")

    /// Skip connection DSL support.
    let SkipConnection(inner) = new SkipConnection(inner)

    let sequential =
        Sequential [|

                // input layer
            Linear(
                Model.inputSize,
                hiddenSize) :> Model
            ReLU()
            Dropout(dropoutRate)

                // hidden layers
            for _ = 1 to numHiddenLayers do
                SkipConnection(
                    Sequential(
                        Linear(
                            hiddenSize,
                            hiddenSize),
                        ReLU(),
                        Dropout(dropoutRate)))

                // output layer
            Linear(
                hiddenSize,
                Model.outputSize)
        |]

    do
        this.RegisterComponents()
        this.register_buffer("isGameLevel", tensor(isGameLevel))   // metadata
        this.``to``(device) |> ignore

    /// Device on which this model is stored.
    member _.Device = device

    /// Indicates whether this model was trained on game-level
    /// payoffs
    member _.IsGameLevel =
        this.get_buffer("isGameLevel").item<bool>()

    /// Applies the given input to this model.
    override _.forward(input) = 
        sequential.forward(input)

module AdvantageModel =

    /// Standard hidden size.
    let standardHiddenSize = 1080   // inherited from original deal-level model

    /// Loads a model for inference.
    let load hiddenSize numHiddenLayers device (path : string) =
        let model =
            new AdvantageModel(
                false,   // will be set correctly below
                hiddenSize,
                numHiddenLayers,
                0.0,
                device)
        model.load(path) |> ignore
        model.eval()
        model

    /// Gets advantages for the given info sets.
    let getAdvantages infoSets (model : AdvantageModel) =
        assert(not model.training)
        use _ = torch.no_grad()
        use input =
            let encoded =
                infoSets
                    |> Array.map (
                        Encoding.encode
                            >> Encoding.toFloat32Array)
                    |> array2D
            tensor(encoded, device = model.Device)
        input --> model
