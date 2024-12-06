namespace Hearts.DeepCfr

open PlayingCards
open Hearts

type InfoSetKey =
    {
        Hand : Hand
        Deal : ClosedDeal
    }

module InfoSetKey =

    let create hand deal =
        {
            Hand = hand
            Deal = deal
        }

module Card =

    let private minRank =
        Seq.min Enum.getValues<Rank>

    let toIndex (card : Card) =
        (int card.Suit * Rank.numRanks)
            + int card.Rank - int minRank

module Encoding =

    let private encodePlayer player =
        [|
            for seat in Enum.getValues<Seat> do
                if seat = player then 1.0f
                else 0.0f
        |]

    let encodeCardValues pairs =
        let valueMap =
            pairs
                |> Seq.map (fun (card, value) ->
                    Card.toIndex card, value)
                |> Map
        [|
            for index = 0 to Card.allCards.Length - 1 do
                valueMap
                    |> Map.tryFind index
                    |> Option.defaultValue 0.0f
        |]

    let private encodeCards cards =
        cards
            |> Seq.map (fun card -> card, 1.0f)
            |> encodeCardValues

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

    let private encodeVoids player voids =
        [|
            for suit in Enum.getValues<Suit> do
                for seat in Enum.getValues<Seat> do
                    if seat <> player then
                        if Set.contains (seat, suit) voids then
                            1.0f
                        else 0.0f
        |]

    let private encodeScore score =
        score.ScoreMap.Values
            |> Seq.map float32
            |> Seq.toArray

    let encodedLength =
        let nCards = Card.allCards.Length
        Seat.numSeats                                 // current player
            + nCards                                  // current player's hand
            + nCards                                  // other unplayed cards
            + ((Seat.numSeats - 1) * nCards)          // current trick
            + (Suit.numSuits * (Seat.numSeats - 1))   // voids
            + Seat.numSeats                           // score

    let encode infoSetKey =
        let deal = infoSetKey.Deal
        let hand = infoSetKey.Hand
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
