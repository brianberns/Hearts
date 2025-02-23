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

    /// Encodes each card in the given current trick as
    /// a one-hot vector in the deck size and then concatenates
    /// those vectors.
    let private encodeTrick trick =
        let cards =
            trick.Cards
                |> List.rev
                |> List.toArray
        assert(cards.Length < Seat.numSeats)
        [|
            for iCard = 0 to Seat.numSeats - 2 do
                yield!
                    if iCard < cards.Length then
                        Some cards[iCard]
                    else None
                    |> Option.toArray
                    |> encodeCards
        |]

    /// Encodes the given voids as a multi-hot vector in the
    /// number of suits times the number of seats.
    let private encodeVoids player voids =
        [|
            for suit in Enum.getValues<Suit> do
                let seats =
                    Seat.cycle player |> Seq.skip 1
                for seat in seats do
                    if Set.contains (seat, suit) voids then
                        1uy
                    else 0uy
        |]

    /// Encodes the given score as a vector in the number
    /// of seats.
    let private encodeScore player score =
        assert(score.ScoreMap.Count = Seat.numSeats)
        [|
            for seat in Seat.cycle player do
                assert(
                    Seq.forall (fun pt ->
                        pt <= int System.Byte.MaxValue)
                            score.ScoreMap.Values)
                byte score.ScoreMap[seat]
        |]

    /// Total encoded length of an info set (hand + deal).
    let encodedLength =
        Card.numCards                                 // current player's hand
            + Card.numCards                           // other unplayed cards
            + ((Seat.numSeats - 1) * Card.numCards)   // current trick
            + ((Seat.numSeats - 1) * Suit.numSuits)   // voids
            + Seat.numSeats                           // score

    /// Encodes the given info set (hand + deal) as a vector.
    let encode (hand : Hand) deal : Encoding =
        let unseen = deal.UnplayedCards - hand
        let trick = ClosedDeal.currentTrick deal
        let player = Trick.currentPlayer trick
        let encoded =
            [|
                yield! encodeCards hand                // current player's hand
                yield! encodeCards unseen              // unplayed cards not in current player's hand
                yield! encodeTrick trick               // current trick
                yield! encodeVoids player deal.Voids   // voids
                yield! encodeScore player deal.Score   // score
            |]
        assert(encoded.Length = encodedLength)
        encoded
