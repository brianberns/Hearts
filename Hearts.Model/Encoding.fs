namespace Hearts.Model

open PlayingCards
open Hearts

module Card =

    /// Rank of lowest card in the deck.
    let private minRank =
        Seq.min Enum.getValues<Rank>

    /// Converts the given card to an integer, 0..N-1,
    /// where N is number of cards in the deck.
    let toIndex (card : Card) =
        let index =
            (int card.Suit * Rank.numRanks)
                + int card.Rank - int minRank
        assert(index >= 0)
        assert(index < Card.numCards)
        index

/// Encoded value for input to a model.
type Encoding = byte[]

module Encoding =

    /// Encodes the given (card, value) pairs as a
    /// vector in the deck size.
    let inline encodeCardValues pairs =
        let valueMap =
            pairs
                |> Seq.map (fun (card, value) ->
                    Card.toIndex card, value)
                |> Map
        [|
            for index = 0 to Card.numCards - 1 do
                valueMap
                    |> Map.tryFind index
                    |> Option.defaultValue
                        LanguagePrimitives.GenericZero   // encode to input type
        |]

    /// Encodes the given cards as a multi-hot vector
    /// in the deck size.
    let private encodeCards cards =
        cards
            |> Seq.map (fun card -> card, 1uy)
            |> encodeCardValues

    /// Encodes the given exchange directon as a one-hot
    /// vector in the number of seats.
    let private encodeExchangeDirection dir =
        [|
            for d in Enum.getValues<ExchangeDirection> do
                if d = dir then 1uy
                else 0uy
        |]

    /// Encodes the given pass as a multi-hot vector in
    /// the deck size.
    let private encodePass (passOpt : Option<Pass>) =
        let cards =
            passOpt
                |> Option.defaultValue Set.empty
        assert(cards.Count <= Pass.numCards)
        encodeCards cards

    let private encodeSeat player seatOpt =
        [|
            for seat in Seat.cycle player do
                if seatOpt = Some seat then 1uy
                else 0uy
        |]

    let private encodeTrick player trickOpt =
        [|
            yield!
                trickOpt
                    |> Option.map _.Leader
                    |> encodeSeat player

            let cards =
                trickOpt
                    |> Option.map (fun trick ->
                        trick.Cards
                            |> List.rev
                            |> List.toArray)
                    |> Option.defaultValue Array.empty
            for iCard = 0 to Seat.numSeats - 1 do
                yield!
                    if iCard < cards.Length then
                        Some cards[iCard]
                    else None
                    |> Option.toArray
                    |> encodeCards
        |]

    let private encodeTricks player deal =
        let tricks =
            ClosedDeal.tricks deal
                |> Seq.toArray
        assert(tricks.Length < ClosedDeal.numCardsPerHand)
        [|
            for iTrick = 0 to ClosedDeal.numCardsPerHand - 2 do
                let trickOpt =
                    if iTrick < tricks.Length then
                        Some tricks[iTrick]
                    else None
                yield! encodeTrick player trickOpt
        |]

    /// Total encoded length of an info set.
    let encodedLength =
        let trickLength =
            Seat.numSeats                           // trick leader
                + (Seat.numSeats * Card.numCards)   // cards played on trick
        Card.numCards                               // current player's hand
            + ExchangeDirection.numDirections       // exchange direction
            + Card.numCards                         // outgoing pass
            + Card.numCards                         // incoming pass
            + ((ClosedDeal.numCardsPerHand - 1)     // tricks
                * trickLength)

    /// Encodes the given info set as a vector.
    let encode infoSet : Encoding =
        let encoded =
            [|
                yield! encodeCards infoSet.Hand             // current player's hand
                yield! encodeExchangeDirection              // exchange direction
                    infoSet.Deal.ExchangeDirection
                yield! encodePass infoSet.OutgoingPassOpt   // outgoing pass
                yield! encodePass infoSet.IncomingPassOpt   // incoming pass
                yield! encodeTricks                         // tricks
                    infoSet.Player infoSet.Deal
            |]
        assert(encoded.Length = encodedLength)
        encoded
