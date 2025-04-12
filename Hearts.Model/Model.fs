namespace Hearts.Model

open TorchSharp
open type torch
open type torch.nn

open PlayingCards
open Hearts

/// Neural network that maps an input tensor to an output
/// tensor.
type Model = Module<Tensor, Tensor>

/// Skip connection.
type SkipConnection(inner : Model) as this =
    inherit Model($"{inner.GetName()}Skip")

    do this.register_module("inner", inner)

    override _.forward(input) =
        use x = input --> inner
        x + input

[<AutoOpen>]
module ModelAuto =

    /// Skip connection.
    let SkipConnection(inner) = new SkipConnection(inner)

/// Model used for learning advantages.
type AdvantageModel(
    name,
    inputSize : int,
    hiddenSize : int,
    numHiddenLayers : int,
    outputSize : int,
    device : torch.Device) as this =
    inherit Model(name)

    let mutable curDevice = device

    let sequential =
        Sequential [|

                // input layer
            Linear(
                inputSize,
                hiddenSize,
                device = device) :> Model
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
                outputSize,
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

module AdvantageModel =

    /// Gets advantages for the given info sets.
    let getAdvantages
        infoSets
        (encode : InformationSet -> Encoding)
        (model : AdvantageModel) =

        assert(
            infoSets
                |> Seq.map _.LegalActionType
                |> Seq.distinct
                |> Seq.length = 1)

        use _ = torch.no_grad()
        use input =
            let encoded =
                infoSets
                    |> Array.map (
                        encode >> Encoding.toFloat32)
                    |> array2D
            tensor(encoded, device = model.Device)
        input --> model

/// Model used for learning exchange advantages.
type ExchangeModel(hiddenSize, numHiddenLayers, device) =
    inherit AdvantageModel(
        "Exchange",
        Encoding.Exchange.encodedLength,
        hiddenSize,
        numHiddenLayers,
        Card.numCards,
        device)

/// Model used for learning playout advantages.
type PlayoutModel(hiddenSize, numHiddenLayers, device) =
    inherit AdvantageModel(
        "Playout",
        Encoding.Playout.encodedLength,
        hiddenSize,
        numHiddenLayers,
        Card.numCards,
        device)

/// Model used for learning Hearts advantages.
type HeartsModel =
    {
        ExchangeModel : ExchangeModel
        PlayoutModel : PlayoutModel
    }

    /// Switches the model to eval mode.
    member this.eval() =
        this.ExchangeModel.eval()
        this.PlayoutModel.eval()

    /// Cleanup.
    member this.Dispose() =
        this.ExchangeModel.Dispose()
        this.PlayoutModel.Dispose()

    interface System.IDisposable with

        /// Cleanup.
        member this.Dispose() = this.Dispose()

module HeartsModel =

    /// Creates a Hearts model.
    let create exchangeModel playoutModel =
        {
            ExchangeModel = exchangeModel
            PlayoutModel = playoutModel
        }
