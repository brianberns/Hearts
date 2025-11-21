namespace Hearts.Learn

open PlayingCards
open Hearts

open Trickster
open Trickster.Bots
open TestBots

module Trickster =

    let private toString cards =
        cards
            |> Seq.map (fun (card : Card) ->
                $"{card.Rank.Char}{card.Suit.Letter}")
            |> String.concat ""

    /// Trickster Hearts bot.
    let player =

        let bot =
            let options =
                cloud.HeartsOptions(
                    passing = cloud.HeartsPassing.LeftRightAcrossKeep)
            HeartsBot(
                options,
                cloud.Suit.Unknown)

        let act infoSet =

            let legalActions = infoSet.LegalActions
            if legalActions.Length = 1 then
                Array.exactlyOne legalActions
            else

                let hand = infoSet.Hand
                let deal = infoSet.Deal
                let players =
                    [|
                        let cardsTakenMap =
                            deal
                                |> ClosedDeal.tricks
                                |> Seq.collect Trick.plays
                                |> Seq.groupBy fst
                                |> Seq.map (fun (seat, plays) ->
                                    seat, Seq.map snd plays)
                                |> Map
                        for seat in Seat.cycle infoSet.Player do
                            let hand =
                                if seat = infoSet.Player then
                                    toString hand
                                else ""
                            let cardsTaken =
                                cardsTakenMap
                                    |> Map.tryFind seat
                                    |> Option.map toString
                                    |> Option.defaultValue ""
                            TestPlayer(
                                hand = hand,
                                handScore = deal.Score[seat],
                                cardsTaken = cardsTaken)
                    |]

                let trick =
                    infoSet.Deal.CurrentTrickOpt
                        |> Option.map (fun trick ->
                            trick
                                |> Trick.plays
                                |> Seq.map snd
                                |> toString)
                        |> Option.defaultValue ""

                let notLegal =
                    (hand, legalActions)
                        ||> Seq.fold (fun hand card ->
                            assert(hand.Contains(card))
                            hand.Remove(card))
                        |> toString

                let card =
                    match infoSet.LegalActionType with
                        | ActionType.Pass ->
                            let cardState =
                                cloud.SuggestPassState(
                                    hand = cloud.Hand(toString hand),
                                    passCount = 1)
                            bot.SuggestPass(cardState)
                                |> Seq.exactlyOne
                        | ActionType.Play ->
                            let cardState =
                                TestCardState(
                                    bot,
                                    players,
                                    trick,
                                    notLegal)
                            bot.SuggestNextCard(cardState)
                let rank = enum<Rank>(int card.rank)
                let suit =
                    match card.suit with
                        | cloud.Suit.Clubs -> Suit.Clubs
                        (*
                        | cloud.Suit.Diamonds -> Suit.Diamonds
                        *)
                        | cloud.Suit.Hearts -> Suit.Hearts
                        | cloud.Suit.Spades -> Suit.Spades
                        | _ -> failwith "Unexpected"
                Card(rank, suit)

        { Act = act }
