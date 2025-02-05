namespace Hearts.Learn

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

module Encoding =

    /// Encodes the given player's seat as a one-hot
    /// vector in the total number of seats.
    let private encodePlayer player =
        [|
            for seat in Enum.getValues<Seat> do
                if seat = player then 1.0f
                else 0.0f
        |]

    /// Encodes the given (card, value) pairs as a
    /// vector in the deck size.
    let encodeCardValues pairs =
        let valueMap =
            pairs
                |> Seq.map (fun (card, value) ->
                    Card.toIndex card, value)
                |> Map
        [|
            for index = 0 to Card.numCards - 1 do
                valueMap
                    |> Map.tryFind index
                    |> Option.defaultValue 0.0f
        |]

    /// Encodes the given cards as a multi-hot vector
    /// in the deck size.
    let private encodeCards cards =
        cards
            |> Seq.map (fun card -> card, 1.0f)
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

    /// Encodes the given voids for the given player as
    /// a multi-hot vector in the number of suits times
    /// the number of seats.
    let private encodeVoids player voids =
        [|
            for suit in Enum.getValues<Suit> do
                for seat in Enum.getValues<Seat> do
                    if seat <> player then
                        if Set.contains (seat, suit) voids then
                            1.0f
                        else 0.0f
        |]

    /// Encodes the given score as a vector in the number
    /// of seats.
    let private encodeScore score =
        assert(score.ScoreMap.Count = Seat.numSeats)
        score.ScoreMap.Values
            |> Seq.map float32
            |> Seq.toArray

    /// Total encoded length of an info set (hand + deal).
    let encodedLength =
        Seat.numSeats                                 // current player
            + Card.numCards                           // current player's hand
            + Card.numCards                           // other unplayed cards
            + ((Seat.numSeats - 1) * Card.numCards)   // current trick
            + (Suit.numSuits * (Seat.numSeats - 1))   // voids
            + Seat.numSeats                           // score

    /// Encodes the given info set as a vector.
    let encode (hand : Hand) deal =
        let otherUnplayed = deal.UnplayedCards - hand
        let trick = ClosedDeal.currentTrick deal
        let player = Trick.currentPlayer trick
        let encoded =
            [|
                yield! encodePlayer player             // current player
                yield! encodeCards hand                // current player's hand
                yield! encodeCards otherUnplayed       // other unplayed cards
                yield! encodeTrick trick               // current trick
                yield! encodeVoids player deal.Voids   // voids
                yield! encodeScore deal.Score          // score
            |]
        assert(encoded.Length = encodedLength)
        encoded
