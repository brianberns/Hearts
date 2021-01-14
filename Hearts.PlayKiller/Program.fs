namespace Hearts.PlayKiller

open PlayingCards
open Hearts

module Naive =

    let makePass deal _ =

        let hand = deal |> OpenDeal.currentHand

        let cards =

            let spadeCandidates =
                [| "QS"; "KS"; "AS" |]
                    |> Seq.map Card.fromString
                    |> Seq.where (fun card ->
                        hand.Contains(card))
                    |> set

            let clubCandidates =
                let clubCards =
                    hand
                        |> Seq.where (fun card ->
                            card.Suit = Suit.Clubs)
                        |> Seq.toArray
                if clubCards.Length = 1 then
                    clubCards
                else
                    Array.empty

            let diamondCandidates =
                let diamondCards =
                    hand
                        |> Seq.where (fun card ->
                            card.Suit = Suit.Diamonds)
                        |> Seq.toArray
                if diamondCards.Length = 1 then
                    diamondCards
                else
                    Array.empty

            let heartCandidates =
                hand
                    |> Seq.where (fun card ->
                        card.Suit = Suit.Hearts)
                    |> Seq.sortDescending
                    |> Seq.toArray

            [|
                yield! spadeCandidates
                yield! clubCandidates
                yield! diamondCandidates
                yield! heartCandidates
            |] |> Seq.truncate 3 |> set

        let moreCards =
            hand
                |> Seq.where (fun card ->
                    cards
                        |> Set.contains card
                        |> not)
                |> Seq.sortByDescending (fun card ->
                    card.Rank)
                |> Seq.take (3 - cards.Count)
                |> Set

        Set.union cards moreCards

    let makePlay deal _ =

        let hand = deal |> OpenDeal.currentHand
        let trick =
            deal.ClosedDeal |> ClosedDeal.currentTrick
        let legalPlays =
            deal.ClosedDeal
                |> ClosedDeal.legalPlays hand
                |> Seq.toArray

        /// Sluffs least desirable card.
        let sluff () =
            legalPlays
                |> Seq.maxBy (fun card ->
                    match card.Rank, card.Suit with
                        | Rank.Queen, Suit.Spades -> 3, card.Rank
                        | Rank.King, Suit.Spades
                        | Rank.Ace, Suit.Spades -> 2, card.Rank
                        | rank, Suit.Hearts -> 1, rank
                        | rank, _ -> 0, rank)

        match trick.SuitLedOpt with

                // leading
            | None ->

                    // choose lowest legal card
                let card =
                    legalPlays
                        |> Seq.minBy (fun card -> card.Rank)

                    // ... unless it's the QS
                if card.String = "QS" && legalPlays.Length > 0 then
                    legalPlays
                        |> Seq.where (fun card -> card.String <> "QS")
                        |> Seq.minBy (fun card -> card.Rank)
                else card

                // following
            | Some suitLed ->

                    // can follow suit?
                let canFollowSuit =
                    legalPlays
                        |> Seq.map (fun card ->
                            card.Suit = suitLed)
                        |> Seq.distinct
                        |> Seq.exactlyOne

                    // no points yet on trick?
                let nPoints =
                    trick.Cards
                        |> Seq.sumBy (fun card -> card |> Card.pointValue)
                if nPoints = 0 then
                    if canFollowSuit then
                        match suitLed with
                            | Suit.Spades ->

                                    // play QS on (AK)S winner
                                let highSpadePlayed =
                                    trick.Cards
                                        |> Seq.exists (fun card ->
                                            card.Rank >= Rank.King)
                                let qs = Card.fromString "QS"
                                let hasQS = legalPlays |> Seq.contains qs
                                if highSpadePlayed && hasQS then
                                    qs

                                    // safe to take trick with highest non-Q?
                                elif trick.Cards.Length = 3 then
                                    legalPlays
                                        |> Seq.sortDescending
                                        |> Seq.where (fun card ->
                                            card.Rank <> Rank.Queen)
                                        |> Seq.tryHead
                                        |> Option.defaultValue qs

                                    // play highest <Q if possible
                                else
                                    legalPlays
                                        |> Seq.sortDescending
                                        |> Seq.where (fun card ->
                                            card.Rank < Rank.Queen)
                                        |> Seq.tryHead
                                        |> Option.defaultWith (fun () ->
                                            legalPlays |> Seq.max)

                                // safe to play highest card
                            | Suit.Clubs
                            | Suit.Diamonds ->
                                legalPlays |> Seq.max

                            | _ -> failwith "Unexpected"

                    else sluff ()
                elif canFollowSuit then

                        // play highest trick loser, if possible
                    let losers =
                        legalPlays
                            |> Seq.where (fun card ->
                                let highCardOpt =
                                    trick
                                        |> Trick.addPlay card
                                        |> Trick.highCardOpt
                                highCardOpt <> Some card)
                            |> Seq.toArray
                    if losers.Length > 0 then
                        losers |> Seq.max

                        // sitting last, play highest safe trick winner
                    elif trick.Cards.Length = 3 then
                        legalPlays
                            |> Seq.sortDescending
                            |> Seq.where (fun card ->
                                card.String <> "QS")
                            |> Seq.tryHead
                            |> Option.defaultWith (fun () ->
                                legalPlays |> Seq.max)

                        // play lowest trick winner
                    else
                        legalPlays
                            |> Seq.sort
                            |> Seq.head

                else sluff ()

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
