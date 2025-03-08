namespace Hearts.Learn

open PlayingCards
open Hearts

module Tournament =

    /// Creates and plays one deal.
    let playDeal (playerMap : Map<_, _>) deal =

        let rec loop deal score =
            let deal =
                let card =
                    let infoSet = OpenDeal.currentInfoSet deal
                    playerMap[infoSet.Player].Play infoSet
                OpenDeal.addPlay card deal
            match Game.tryUpdateScore deal score with
                | Some score -> score
                | None -> loop deal score

        loop deal Score.zero

    /// Plays the given number of deals.
    let playDeals rng numDeals playerMap =
        OpenDeal.generate rng numDeals (
            playDeal playerMap)
            |> Seq.reduce (+)

    /// Runs a tournament between two players.
    let run rng champion challenger =
        let challengerSeat = Seat.South
        let playerMap =
            Enum.getValues<Seat>
                |> Seq.map (fun seat ->
                    let player =
                        if seat = challengerSeat then challenger
                        else champion
                    seat, player)
                |> Map
        let score =
            playDeals rng
                settings.NumEvaluationDeals
                playerMap
        let payoff =
            (ZeroSum.getPayoff score)[int challengerSeat]
                / float32 settings.NumEvaluationDeals

        if settings.Verbose then
            printfn "\nTournament:"
            for (KeyValue(seat, points)) in score.ScoreMap do
                printfn $"   %-6s{string seat}: {points}"
            printfn $"   Payoff: %0.5f{payoff}"

        payoff

module Trickster =

    // open Trickster.Bots
    // open Trickster.cloud
    // open TestBots

    let private toString cards =
        cards
            |> Seq.map (fun (card : Card) ->
                $"{card.Rank.Char}{card.Suit.Letter}")
            |> String.concat ""

    /// Trickster Hearts bot.
    let player =

        let bot =
            let options =
                Trickster.cloud.HeartsOptions(
                    passing = Trickster.cloud.HeartsPassing.LeftRightAcrossKeep)
            Trickster.Bots.HeartsBot(
                options,
                Trickster.cloud.Suit.Unknown)

        let play infoSet =

            let actionType, legalActions =
                InformationSet.legalActions infoSet
            if legalActions.Length = 1 then
                Seq.head legalActions
            else

                let hand = infoSet.Hand
                let deal = infoSet.Deal
                let players =
                    [|
                        let curPlayer =
                            ClosedDeal.currentPlayer deal
                        let cardsTakenMap =
                            deal
                                |> ClosedDeal.tricks
                                |> Seq.collect Trick.plays
                                |> Seq.groupBy fst
                                |> Seq.map (fun (seat, plays) ->
                                    seat, Seq.map snd plays)
                                |> Map
                        for seat in Seat.cycle curPlayer do
                            let hand =
                                if seat = curPlayer then
                                    toString hand
                                else ""
                            let cardsTaken =
                                cardsTakenMap
                                    |> Map.tryFind seat
                                    |> Option.map toString
                                    |> Option.defaultValue ""
                            TestBots.TestPlayer(
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
                    match actionType with
                        | Pass ->
                            let cardState =
                                Trickster.cloud.SuggestPassState(
                                    hand = Trickster.cloud.Hand(toString hand),
                                    passCount = 1)
                            bot.SuggestPass(cardState)
                                |> Seq.exactlyOne
                        | Play ->
                            let cardState =
                                TestBots.TestCardState<Trickster.cloud.HeartsOptions>(
                                    bot,
                                    players,
                                    trick,
                                    notLegal)
                            bot.SuggestNextCard(cardState)
                let rank = enum<Rank>(int card.rank)
                let suit =
                    match card.suit with
                        | Trickster.cloud.Suit.Clubs -> Suit.Clubs
                        | Trickster.cloud.Suit.Diamonds -> Suit.Diamonds
                        | Trickster.cloud.Suit.Hearts -> Suit.Hearts
                        | Trickster.cloud.Suit.Spades -> Suit.Spades
                        | _ -> failwith "Unexpected"
                Card(rank, suit)

        { Play = play }
