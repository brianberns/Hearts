namespace Hearts.PlayKiller

open PlayingCards
open Hearts

module Naive =

    let makePass deal _ =

        let hand = deal |> OpenDeal.currentHand

            // determine spades to pass
        let spadesToPass =

            let spades =
                hand
                    |> Seq.where (fun card ->
                        card.Suit = Suit.Spades)
                    |> Seq.toArray
            let nSpades = spades.Length

            let highSpades =
                spades
                    |> Array.where (fun card ->
                        card.Rank >= Rank.Queen)
            let nHighSpades = highSpades.Length

            if nSpades > 4
                || (nSpades = 4 && nHighSpades = 1) then
                Set.empty
            else
                Set highSpades
                
            // determine other cards to pass
        let othersToPass =
            let nOthers =
                Exchange.numCards - spadesToPass.Count
            hand
                |> Seq.where (fun card ->
                    card.Suit <> Suit.Spades)
                |> Seq.sortDescending
                |> Seq.take nOthers
                |> Set

        spadesToPass + othersToPass

    let cardQS = Card(Rank.Queen, Suit.Spades)

    let makePlay deal _ =

        let hand = deal |> OpenDeal.currentHand
        let legalPlays =
            deal.ClosedDeal
                |> ClosedDeal.legalPlays hand
                |> Seq.toArray
        let trick =
            deal.ClosedDeal |> ClosedDeal.currentTrick

        let cardOpt =
            match trick.Cards.Length with
                | 0 ->
                    if deal.ClosedDeal.PlayedCards.Contains(cardQS) then
                        None
                    else
                        let highSpades, lowSpades =
                            hand
                                |> Seq.where (fun card ->
                                    card.Suit = Suit.Spades)
                                |> Seq.toArray
                                |> Array.partition (fun card ->
                                    card.Rank >= Rank.Queen)
                        if highSpades.Length = 0 && lowSpades.Length > 0 then
                            let card = lowSpades |> Seq.max
                            assert(legalPlays |> Seq.contains card)
                            Some card
                        else None
                | 3 ->
                    let isTrickWinner card =
                        let trick = trick |> Trick.addPlay card
                        snd trick.HighPlayOpt.Value = card
                    let winners, nonWinners =
                        legalPlays
                            |> Array.partition isTrickWinner
                    if nonWinners.Length > 0 then
                        nonWinners
                            |> Seq.maxBy (fun card ->
                                card |> Card.pointValue, card.Rank)
                            |> Some
                    else
                        let winners =
                            winners
                                |> Array.where (fun card -> card <> cardQS)
                        if winners.Length > 0 then
                            winners |> Seq.max |> Some
                        else None
                | _ -> None

        cardOpt
            |> Option.defaultWith (fun () ->
                legalPlays
                    |> Seq.sort
                    |> Seq.head)

    let player =
        Player.create makePass makePlay

module Program =

    [<EntryPoint>]
    let main argv =

        let Not a =
            If (a, BoolLiteral false, BoolLiteral true)

        let Or (a, b) =
            If (a, BoolLiteral true, b)

        let And (a, b) =
            If (a, b, BoolLiteral false)

        let GreaterThanOrEqualTo (x, y) =
            Or (
                Equal (x, y),
                GreaterThan (x, y))

        let passExpr =

            let spadesToPass =

                let spades =
                    Where (
                        MyHand,
                        Lambda (
                            TCard,
                            Equal (
                                CardSuit (Variable 0),
                                SuitLiteral Suit.Spades)))
                let nSpades =
                    CardSetCount spades

                let highSpades =
                    Where (
                        spades,
                        Lambda (
                            TCard,
                            GreaterThanOrEqualTo (
                                CardRank (Variable 0),
                                RankLiteral Rank.Queen)))
                let nHighSpades =
                    CardSetCount highSpades

                If (
                    Or (
                        GreaterThanOrEqualTo (nSpades, IntLiteral 4),
                        And (
                            Equal (nSpades, IntLiteral 4),
                            Equal (nHighSpades, IntLiteral 1))),
                    CardSetLiteral Set.empty,
                    highSpades)

            let othersToPass =
                TakeDescending (
                    Where (
                        MyHand,
                        Lambda (
                            TCard,
                            Not (
                                Equal (
                                    CardSuit (Variable 0),
                                    SuitLiteral Suit.Spades)))),
                    Subtract (
                        IntLiteral 3,
                        CardSetCount spadesToPass))

            Union (spadesToPass, othersToPass)

        passExpr
            |> Expr.typeOf
            |> printfn "%A"

        let deal =
            let rng = System.Random(0)
            let deck = Deck.shuffle rng
            let deal = OpenDeal.fromDeck Seat.South ExchangeDirection.Hold deck
            deal |> OpenDeal.startPlay
        passExpr
            |> Expr.eval deal
            |> printfn "%A"

        (*
        try
            let (ScoreMap scoreMap) = Killer.run Naive.player
            for (KeyValue(seat, points)) in scoreMap do
                printfn "%A: %d" seat points
        with ex ->
            printfn "%s" ex.Message
            printfn "%s" ex.StackTrace
        *)

        0
