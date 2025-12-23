namespace Hearts

open PlayingCards
open Hearts

module Tournament =

    /// Runs a tournament between two players.
    let run rng inParallel numGames champion challenger =

            // prepare players
        let challengerSeat = Seat.South
        let playerMap =
            Enum.getValues<Seat>
                |> Seq.map (fun seat ->
                    let player =
                        if seat = challengerSeat then challenger
                        else champion
                    seat, player)
                |> Map

            // run games
        let score =
            Game.playGames rng inParallel numGames (
                Game.playGame rng playerMap)
                |> Seq.map _.Score
                |> Seq.reduce (+)

            // compute payoff for challenger
        let payoff =
            (Game.getPayoffs score)[int challengerSeat]
                / float32 numGames
        score, payoff
