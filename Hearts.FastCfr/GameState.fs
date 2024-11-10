namespace Hearts.FastCfr

open System
open System.IO

open PlayingCards
open Hearts
open FastCfr

module GameState =

#if FABLE_COMPILER
    type Collections.Generic.List<'t> with
        member this.WriteByte(item) = this.Add(item)
        member this.Length = this.Count
        member this.Capacity = this.Count
#endif

    let getInfoSetKey (hand : Hand) deal =
#if FABLE_COMPILER
        let stream = ResizeArray<byte>(
                    Card.allCards.Length                          // unplayed cards
                        + 1                                       // trick leader
                        + (2 * (Seat.numSeats - 1))               // trick cards
                        + (Suit.numSuits * (Seat.numSeats - 1))   // voids
                        + 1)                                      // score
#else
        use stream =
            new MemoryStream(
                Card.allCards.Length                          // unplayed cards
                    + 1                                       // trick leader
                    + (2 * (Seat.numSeats - 1))               // trick cards
                    + (Suit.numSuits * (Seat.numSeats - 1))   // voids
                    + 1)                                      // score
#endif

            // hand and other unplayed cards
        for card in Card.allCards do
            stream.WriteByte(
                if hand.Contains(card) then
                    assert(deal.UnplayedCards.Contains(card))
                    2uy
                elif deal.UnplayedCards.Contains(card) then 1uy
                else 0uy)

            // current trick
        let trick = ClosedDeal.currentTrick deal
        stream.WriteByte(uint8 trick.Leader)
        let cards =
            trick.Cards
                |> List.rev
                |> List.toArray
        assert(cards.Length < Seat.numSeats)
        for iCard = 0 to Seat.numSeats - 2 do
            if iCard < cards.Length then
                stream.WriteByte(uint8 cards[iCard].Rank)
                stream.WriteByte(uint8 cards[iCard].Suit)
            else
                stream.WriteByte(Byte.MaxValue)
                stream.WriteByte(Byte.MaxValue)

            // voids
        let player = Trick.currentPlayer trick
        let unplayedSuits =
            (deal.UnplayedCards - hand)
                |> Seq.map (fun card -> card.Suit)
                |> set
        for suit in Enum.getValues<Suit> do
            let hasUnplayed = unplayedSuits.Contains(suit)
            for seat in Enum.getValues<Seat> do
                if seat <> player then
                    stream.WriteByte(
                        if hasUnplayed
                            && deal.Voids.Contains(seat, suit) then 1uy
                        else 0uy)

            // score
        assert(deal.Score.ScoreMap.Count = Seat.numSeats)
        let seats =
            deal.Score.ScoreMap
                |> Map.toSeq
                |> Seq.where (fun (_, points) -> points > 0)
                |> Seq.map fst
                |> Seq.toArray
        match seats.Length with
            | 0 -> Byte.MaxValue
            | 1 -> uint8 seats[0]
            | _ -> Byte.MaxValue / 2uy
            |> stream.WriteByte

        assert(stream.Length = stream.Capacity)
        stream.ToArray()

    let rec create deal =
        match OpenDeal.tryFinalScore deal with
            | Some score ->
                let otherAvg =
                    (score.ScoreMap
                        |> Map.toSeq
                        |> Seq.where (fun (seat, _) -> seat <> Seat.South)
                        |> Seq.sumBy snd
                        |> float)
                        / float (Seat.numSeats - 1)
                let payoff = otherAvg - float score[Seat.South]
                TerminalGameState.create 0 payoff
                    |> Terminal
            | None ->
                let seat = OpenDeal.currentPlayer deal
                let hand = deal.UnplayedCardMap[seat]
                let playerIdx =
                    if seat = Seat.South then 0
                    else 1
                NonTerminal {
                    ActivePlayerIdx = playerIdx
                    InfoSetKey = getInfoSetKey hand deal.ClosedDeal
                    LegalActions =
                        ClosedDeal.legalPlays hand deal.ClosedDeal
                            |> Seq.toArray
                    AddAction =
                        fun card ->
                            OpenDeal.addPlay card deal
                                |> create
                }
