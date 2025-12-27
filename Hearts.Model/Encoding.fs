namespace Hearts.Model

open MathNet.Numerics.LinearAlgebra

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
type Encoding = Vector<float32>

module Encoding =

    /// Converts the given encoding to float32.
    let toFloat32 (encoding : Encoding) =
        encoding.ToArray()
            |> Array.map float32

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
    let encodeCards cards =
        let flags = Array.zeroCreate Card.numCards
        for index in Seq.map Card.toIndex cards do
            flags[index] <- true   // use mutation for speed
        flags

    /// Encodes the given exchange direction as a one-hot
    /// vector in the number of exchange directions.
    let encodeExchangeDirection dir =
        [|
            for d in Enum.getValues<ExchangeDirection> do
                d = dir
        |]

    /// Encodes the given pass as a multi-hot vector in
    /// the deck size.
    let encodePass passOpt =
        let cards : Pass =
            passOpt
                |> Option.defaultValue Set.empty
        assert(cards.Count <= Pass.numCards)
        encodeCards cards

    let private maxPriorPlays =
        Card.numCards - Seat.numSeats - 1   // 52 - 4 - 1 = 47 plays prior to last card on second-to-last trick

    /// Encodes the given prior plays as pairs of vectors:
    /// * Player: one-hot vector in the number of seats
    /// * Card played: one-hot vector in the deck size
    let encodePlays player tricks =
        let seatPlays =
            tricks
                |> Seq.collect Trick.plays
                |> Seq.toArray
        [|
            for iCard = 0 to maxPriorPlays - 1 do

                    // play has occurred?
                if iCard < seatPlays.Length then
                    let seat, card = seatPlays[iCard]

                        // seat encoding
                    let iSeat = Seat.getIndex seat player
                    yield! Array.init Seat.numSeats (fun i -> i = iSeat)

                        // card encoding
                    yield! encodeCards [| card |]

                else
                    yield! Array.zeroCreate Seat.numSeats
                    yield! Array.zeroCreate Card.numCards
        |]

    /// Total encoded length of an info set.
    let encodedLength =
        Card.numCards                                           // current player's hand
            + ExchangeDirection.numDirections                   // exchange direction
            + Card.numCards                                     // outgoing pass
            + Card.numCards                                     // incoming pass
            + maxPriorPlays * (Seat.numSeats + Card.numCards)   // each prior player and card

    /// Encodes the given info set as a vector.
    let encode infoSet : Encoding =
        let flags =
            [|
                yield! encodeCards infoSet.Hand             // current player's hand
                yield! encodeExchangeDirection              // exchange direction
                    infoSet.Deal.ExchangeDirection
                yield! encodePass infoSet.OutgoingPassOpt   // outgoing pass
                yield! encodePass infoSet.IncomingPassOpt   // incoming pass
                yield! encodePlays                          // each prior player and card
                    infoSet.Player
                    (ClosedDeal.tricks infoSet.Deal)
            |]
        assert(flags.Length = encodedLength)
        flags
            |> Array.map (fun flag -> if flag then 1f else 0f)
            |> SparseVector.ofArray
