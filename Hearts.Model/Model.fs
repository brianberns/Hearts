namespace Hearts.Model

open TorchSharp
open type torch
open type torch.nn
open FSharp.Core.Operators   // reclaim "float32" and other F# operators

open Hearts
open PlayingCards

/// Neural network that maps an input tensor to an output
/// tensor.
type Network = Module<Tensor, Tensor>

module Network =

    /// Size of a neural network hidden layer.
    let hiddenSize = 192

    /// Size of neural network output.
    let outputSize = Card.numCards

/// Model used for learning advantages.
type AdvantageModel(device : torch.Device) as this =
    inherit Network("AdvantageModel")

    let mutable curDevice = device

    let cardEmbedding =
        Sequential(
            Linear(
                Card.numCards,
                Network.hiddenSize,
                device = device),
            ReLU())

    let voidsEmbedding =
        Sequential(
            Linear(
                int64 (Seat.numSeats * Suit.numSuits),
                Network.hiddenSize,
                device = device),
            ReLU())
            
    let scoreEmbedding =
        Sequential(
            Linear(
                Seat.numSeats,
                Network.hiddenSize,
                device = device),
            ReLU())

    let trunk =
        Sequential(
            Linear(
                int64 (7 * Network.hiddenSize),   // hand, otherUnplayed, trick, trick, trick, voids, score
                Network.hiddenSize,
                device = device),
            ReLU(),
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

        use hand =
            use slice =
                input.narrow(1, 0, Card.numCards)
            slice --> cardEmbedding

        use otherUnplayed =
            use slice =
                input.narrow(1, Card.numCards, Card.numCards)
            slice --> cardEmbedding

        let batchSize = input.shape[0]
        use trick =
            use slice =
                input.narrow(
                    1,
                    int64 (2 * Card.numCards),
                    int64 (3 * Card.numCards))                // batch size, 3 * 52
            use reshaped = slice.reshape(-1, Card.numCards)   // 3 * nEncodings, 52
            use output = reshaped --> cardEmbedding           // 3 * nEncodings, hidden size
            output.view(batchSize, -1)                        // batch size, 3 * hidden size

        use voids =
            use slice =
                input.narrow(
                    1,
                    int64 (5 * Card.numCards),
                    int64 (Suit.numSuits * Seat.numSeats))
            slice --> voidsEmbedding

        use score =
            use slice =
                input.narrow(
                    1,
                    int64 (5 * Card.numCards)
                        + int64 (Suit.numSuits * Seat.numSeats),
                    Seat.numSeats)
            slice --> scoreEmbedding

        use full =
            torch.cat([|
                hand
                otherUnplayed
                trick
                voids
                score
            |], dim = -1)
        full --> trunk

module AdvantageModel =

    /// Gets the advantage for the given info set (hand + deal).
    let getAdvantage hand deal (model : AdvantageModel) =
        use _ = torch.no_grad()
        use input =
            let encoded = Encoding.encode hand deal
            tensor(
                encoded, device = model.Device,
                dtype = ScalarType.Float32)
        use input2d = input.unsqueeze(0)
        input2d --> model
