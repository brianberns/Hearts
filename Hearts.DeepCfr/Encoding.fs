namespace Hearts.DeepCfr

open PlayingCards
open Hearts

module Encoding =

    let private toIndex (card : Card) =
        (int card.Suit * Rank.numRanks) + int card.Rank

    let private encodeCards cards =
        let indexes =
            cards
                |> Seq.map toIndex
                |> set
        [|
            for index = 0 to Card.allCards.Length - 1 do
                if indexes.Contains(index) then 1.0f
                else 0.0f
        |]

    let private encodeTrick (trick : Trick) =
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

    let private encodeScore (score : Score) =
        score.ScoreMap.Values
            |> Seq.map float32
            |> Seq.toArray

    let encodedLength =
        let nCards = Card.allCards.Length
        nCards                                        // current player's hand
            + nCards                                  // other unplayed cards
            + ((Seat.numSeats - 1) * nCards)          // current trick
            + (Suit.numSuits * (Seat.numSeats - 1))   // voids

    let encodeInput (hand : Hand) deal =
        let encoded =
            [|
                    // current player's hand
                yield! encodeCards hand

                    // other unplayed cards
                yield! encodeCards (deal.UnplayedCards - hand)

                    // current trick
                let trick = ClosedDeal.currentTrick deal
                yield! encodeTrick trick

                    // voids
                let player = Trick.currentPlayer trick
                yield! encodeVoids player deal.Voids

                    // score
                yield! encodeScore deal.Score
            |]
        assert(encoded.Length = encodedLength)
        encoded
