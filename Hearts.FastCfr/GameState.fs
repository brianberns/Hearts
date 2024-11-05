namespace Hearts.FastCfr

open System
open PlayingCards
open Hearts
open FastCfr

module GameState =

    let getInfoSetKey (hand : Hand) deal =
        let sb = Text.StringBuilder(29)

            // hand and other unplayed cards
        for card in Card.allCards do
            sb.Append(
                if hand.Contains(card) then
                    assert(deal.UnplayedCards.Contains(card))
                    'o'
                elif deal.UnplayedCards.Contains(card) then 'x'
                else '.')
                |> ignore

            // current trick
        let trick = ClosedDeal.currentTrick deal
        sb.Append(trick.Leader.Char) |> ignore
        let cards =
            trick.Cards
                |> List.rev
                |> List.toArray
        for iCard = 0 to Seat.numSeats - 1 do
            sb.Append(
                if iCard < cards.Length then
                    cards[iCard].String
                else "..")
                |> ignore

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
                    sb.Append(
                        if hasUnplayed
                            && deal.Voids.Contains(seat, suit) then 'x'
                        else '.')
                        |> ignore

            // score
        assert(deal.Score.ScoreMap.Count = Seat.numSeats)
        let withPoints =
            deal.Score.ScoreMap
                |> Map.toSeq
                |> Seq.where (fun (_, points) -> points > 0)
                |> Seq.map fst
                |> Seq.toArray
        match withPoints.Length with
            | 0 -> '0'
            | 1 -> withPoints[0].Char
            | _ -> 'x'
            |> sb.Append
            |> ignore

        sb.ToString()

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
