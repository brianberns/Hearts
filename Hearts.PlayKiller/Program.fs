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
            if trick.Cards.Length = 0 then
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
            else None

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
        try
            let (ScoreMap scoreMap) = Killer.run Naive.player
            for (KeyValue(seat, points)) in scoreMap do
                printfn "%A: %d" seat points
        with ex ->
            printfn "%s" ex.Message
            printfn "%s" ex.StackTrace
        0
