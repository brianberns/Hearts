namespace Hearts.Learn

open PlayingCards
open Hearts

module Tournament =

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
            Game.playDeals
                rng
                settings.NumEvaluationDeals
                playerMap
        let payoff =
            (ZeroSum.getPayoff score)[int challengerSeat]
                / float32 settings.NumEvaluationDeals

        if settings.Verbose then
            printfn "\nTournament:"
            for (KeyValue(seat, points)) in score.ScoreMap do
                printfn $"   {string seat}: {points}"
            printfn $"   Payoff: %0.5f{payoff}"

        payoff

    /// Random Hearts player.
    let randomPlayer =
        let play hand deal =
            let legalPlays =
                ClosedDeal.legalPlays hand deal
                    |> Seq.toArray
            legalPlays[settings.Random.Next(legalPlays.Length)]
        { Play = play }

module Trickster =

    // open Trickster.Bots
    // open Trickster.cloud
    // open TestBots

    let toString (cards : seq<Card>) =
        cards
            |> Seq.map _.String
            |> String.concat ""

    /// Trickster Hearts bot.
    let player =

        let bot =
            let options =
                Trickster.cloud.HeartsOptions()
            Trickster.Bots.HeartsBot(
                options,
                Trickster.cloud.Suit.Unknown)

        let play (hand : Hand) deal =

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
                            toString cardsTakenMap[seat]
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

            let notLegal =
                let legal =
                    ClosedDeal.legalPlays hand deal
                        |> set
                toString (hand - legal)

            let cardState =
                TestBots.TestCardState<Trickster.cloud.HeartsOptions>(
                    bot,
                    players,
                    trick,
                    notLegal)

            let card =
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
