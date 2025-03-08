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
                    passing = Trickster.cloud.HeartsPassing.None)
            Trickster.Bots.HeartsBot(
                options,
                Trickster.cloud.Suit.Unknown)

        let play infoSet =

            let hand = infoSet.Secret.Hand
            let deal = infoSet.Deal
            let legalPlays =
                ClosedDeal.legalPlays hand deal
                    |> set
            if legalPlays.Count = 1 then
                Seq.head legalPlays
            else

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
                    ClosedDeal.currentTrick deal
                        |> Trick.plays
                        |> Seq.map snd
                        |> toString

                let notLegal = toString (hand - legalPlays)

                let cardState =
                    TestBots.TestCardState<Trickster.cloud.HeartsOptions>(
                        bot,
                        players,
                        trick,
                        notLegal)

                let card = bot.SuggestNextCard(cardState)
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
