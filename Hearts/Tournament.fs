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
        let games =
            Game.playGames rng inParallel numGames (
                Game.playGame rng playerMap)

            // compute results
        let winnerMap =
            games
                |> Seq.collect (Game.findGameWinners)
                |> Seq.groupBy id
                |> Seq.map (fun (seat, group) ->
                    seat, Seq.length group)
                |> Map
        let score =
            Score.ofPoints [|
                for seat in Enum.getValues<Seat> do
                    match Map.tryFind seat winnerMap with
                        | Some wins -> wins
                        | None -> 0
            |]
        let payoff =
            (float32 score[challengerSeat] / float32 numGames)
        score, payoff
