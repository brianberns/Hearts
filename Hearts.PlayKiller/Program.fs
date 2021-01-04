namespace Hearts.PlayKiller

open PlayingCards

open Hearts

module Naive =

    let makePass deal _ =

        let hand = deal |> OpenDeal.currentHand

            // determine spades to pass
        let spadesToPass =
            let spadeRanks =
                hand
                    |> Seq.where (fun card ->
                        card.Suit = Suit.Spades)
                    |> Seq.map (fun card -> card.Rank)
                    |> set
            let nSpades = spadeRanks.Count
            let toPass =
                if nSpades > 4
                    || (nSpades = 4
                        && spadeRanks.Contains(Rank.Queen)) then
                    fun _ -> false
                else
                    fun rank -> rank >= Rank.Queen
            spadeRanks
                |> Seq.where toPass
                |> Seq.map (fun rank -> Card(rank, Suit.Spades))
                |> set
                
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

    let makePlay deal _ =
        let hand = OpenDeal.currentHand deal
        deal.ClosedDeal
            |> ClosedDeal.legalPlays hand
            |> Seq.sort
            |> Seq.head

    let player =
        {
            MakePass = makePass
            MakePlay = makePlay
        }

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
