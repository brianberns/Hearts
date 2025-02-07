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
