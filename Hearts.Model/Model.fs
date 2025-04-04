namespace Hearts.Model

open TorchSharp
open type torch
open type torch.nn

open PlayingCards

/// Neural network that maps an input tensor to an output
/// tensor.
type Network = Module<Tensor, Tensor>

type SkipConnection(inner : Network) as this =
    inherit Network($"{inner.GetName()}Skip")

    do this.register_module("inner", inner)

    override _.forward(input) =
        use x = input --> inner
        x + input

[<AutoOpen>]
module Network =

    let SkipConnection(inner) = new SkipConnection(inner)

/// Model used for learning exchange advantages.
type ExchangeModel(
    hiddenSize : int,
    numHiddenLayers : int,
    device : torch.Device) as this =
    inherit Network("ExchangeModel")

    let mutable curDevice = device

    let sequential =
        Sequential [|

                // input layer
            Linear(
                Encoding.Exchange.encodedLength,
                hiddenSize,
                device = device) :> Network
            ReLU()
            Dropout()

                // hidden layers
            for _ = 1 to numHiddenLayers do
                SkipConnection(
                    Sequential(
                        Linear(
                            hiddenSize,
                            hiddenSize,
                            device = device),
                        ReLU(),
                        Dropout()))

                // output layer
            Linear(
                hiddenSize,
                Card.numCards,
                device = device)
        |]

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

module ExchangeModel =

    /// Gets advantages for the given exchange info sets.
    let getAdvantages infoSets (model : ExchangeModel) =
        use _ = torch.no_grad()
        use input =
            let encoded =
                infoSets
                    |> Array.map Encoding.Exchange.encode
                    |> array2D
            tensor(
                encoded, device = model.Device,
                dtype = ScalarType.Float32)
        input --> model

/// Model used for learning playout advantages.
type PlayoutModel(
    hiddenSize : int,
    numHiddenLayers : int,
    device : torch.Device) as this =
    inherit Network("PlayoutModel")

    let mutable curDevice = device

    let sequential =
        Sequential [|

                // input layer
            Linear(
                Encoding.Exchange.encodedLength,
                hiddenSize,
                device = device) :> Network
            ReLU()
            Dropout()

                // hidden layers
            for _ = 1 to numHiddenLayers do
                SkipConnection(
                    Sequential(
                        Linear(
                            hiddenSize,
                            hiddenSize,
                            device = device),
                        ReLU(),
                        Dropout()))

                // output layer
            Linear(
                hiddenSize,
                Card.numCards,
                device = device)
        |]

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

module PlayoutModel =

    /// Gets advantages for the given playout info sets.
    let getAdvantages infoSets (model : PlayoutModel) =
        use _ = torch.no_grad()
        use input =
            let encoded =
                infoSets
                    |> Array.map Encoding.Playout.encode
                    |> array2D
            tensor(
                encoded, device = model.Device,
                dtype = ScalarType.Float32)
        input --> model
