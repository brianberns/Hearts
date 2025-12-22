namespace Hearts

open PlayingCards
open Hearts

module Tournament =

    /// Gets per-player's payoffs for the given game score.
    let private getPayoffs gameScore =

            // find high and low scores
        let low = Array.min gameScore.Points
        let high = Array.max gameScore.Points
   
            // game is over?
        if high >= Game.endThreshold then
            gameScore.Points
                |> Array.map (fun x ->
                    if x = low then 1.0f else 0.0f)

        else
                // calculate Softmax terms relative to the low score
            let temperature =
                let volatility = 10.0f   // prevent temperature from dropping to near-zero
                float32 (Game.endThreshold - high) + volatility
            let terms = 
                gameScore.Points
                    |> Array.map (fun pt ->
                        exp (float32 (low - pt) / temperature))

                // normalize
            let total = Array.sum terms
            let payoffs =
                terms
                    |> Array.map (fun term ->
                        let payoff = term / total
                        assert(payoff >= 0.0f && payoff <= 1.0f)
                        payoff)
            payoffs

    /// Computes the payoffs for the given game, if the result
    /// of the current deal is inevitable.
    let private tryGetPayoffs game =
        Game.tryUpdateScore game
            |> Option.map (_.Score >> getPayoffs)

    /// Plays the given number of games.
    let private playGames rng inParallel numGames playerMap =
        Game.playGames rng inParallel numGames (
            Game.playGame playerMap)
            |> Seq.map _.Score
            |> Seq.reduce (+)

    /// Runs a tournament between two players.
    let run rng inParallel numGames champion challenger =
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
            playGames rng inParallel numGames playerMap
        let payoff =
            (getPayoffs score)[int challengerSeat]
                / float32 numGames
        score, payoff
