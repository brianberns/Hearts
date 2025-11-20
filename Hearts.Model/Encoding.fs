namespace Hearts.Model

open System.Collections
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
type Encoding = BitArray

module Encoding =

    /// Converts encoded bits to float32.
    let toFloat32 (bits : BitArray) =
        [|
            for i = 0 to bits.Length - 1 do
                if bits[i] then 1f else 0f
        |]

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

    /// Encodes each card in the given current trick as
    /// a one-hot vector in the deck size and concatenates
    /// those vectors.
    let encodeTrick trickOpt =
        let cards =
            trickOpt
                |> Option.map (_.Cards >> Seq.toArray)
                |> Option.defaultValue Array.empty
        assert(cards.Length < Seat.numSeats)
        [|
            for iCard = 0 to Seat.numSeats - 2 do
                yield!
                    if iCard < cards.Length then
                        Some cards[cards.Length - 1 - iCard]   // unreverse into chronological order
                    else None
                    |> Option.toArray
                    |> encodeCards
        |]

    /// Encodes the given voids as a multi-hot vector in the
    /// number of suits times the number of other seats.
    let encodeVoids player voids =
        let flags =
            Array.zeroCreate ((Seat.numSeats - 1) * Suit.numSuits)
        for (seat, suit) in voids do
            if seat <> player then
                let suitOffset = (Seat.numSeats - 1) * int suit
                let seatOffset =
                    ((int seat - int player - 1) + Seat.numSeats)
                        % Seat.numSeats
                flags[suitOffset + seatOffset] <- true   // use mutation for speed
        flags

    /// Encodes the given score as a multi-hot vector in the
    /// number of seats.
    let encodeScore player score =
        assert(score.Points.Length = Seat.numSeats)
        [|
            for seat in Seat.cycle player do
                score[seat] > 0
        |]

    /// Total encoded length of an info set.
    let encodedLength =
        Card.numCards                                 // current player's hand
            + Card.numCards                           // unplayed cards not in current player's hand
            + ExchangeDirection.numDirections         // exchange direction
            + Card.numCards                           // outgoing pass
            + Card.numCards                           // incoming pass
            + ((Seat.numSeats - 1) * Card.numCards)   // current trick
            + ((Seat.numSeats - 1) * Suit.numSuits)   // voids
            + Seat.numSeats                           // score

    /// Encodes the given info set as a vector.
    let encode infoSet : Encoding =
        let unseen =
            infoSet.Deal.UnplayedCards - infoSet.Hand
        let trickOpt = infoSet.Deal.CurrentTrickOpt
        let encoded =
            BitArray [|
                yield! encodeCards infoSet.Hand             // current player's hand
                yield! encodeCards unseen                   // unplayed cards not in current player's hand
                yield! encodeExchangeDirection              // exchange direction
                    infoSet.Deal.ExchangeDirection
                yield! encodePass infoSet.OutgoingPassOpt   // outgoing pass
                yield! encodePass infoSet.IncomingPassOpt   // incoming pass
                yield! encodeTrick trickOpt                 // current trick
                yield! encodeVoids                          // voids
                    infoSet.Player infoSet.Deal.Voids
                yield! encodeScore                          // score
                    infoSet.Player infoSet.Deal.Score
            |]
        assert(encoded.Length = encodedLength)
        encoded
