namespace Hearts

open PlayingCards
open Hearts

module Tournament =

    /// Gets per-player's payoffs for the given game score.
    let getPayoffs gameScore =

            // find high and low scores
        let low = Array.min gameScore.Points
        let high = Array.max gameScore.Points
   
            // game is over?
        if high >= Game.endThreshold then
            gameScore.Points
                |> Array.map (fun x ->
                    if x = low then 1.0 else 0.0)

        else
                // calculate Softmax terms relative to the low score
            let t =
                let volatility = 10.0   // prevent temperature from dropping to near-zero
                float (Game.endThreshold - high) + volatility
            let terms = 
                gameScore.Points
                    |> Array.map (fun pt ->
                        exp (float (low - pt) / t))

                // normalize
            let total = Array.sum terms
            let payoffs =
                terms
                    |> Array.map (fun term ->
                        let payoff = term / total
                        assert(payoff >= 0.0 && payoff <= 1.0)
                        payoff)
            payoffs

    /// Computes the payoffs for the given game, if the result
    /// of the current deal is inevitable.
    let tryGetPayoffs game =
        Game.tryUpdateScore game
            |> Option.map (_.Score >> getPayoffs)

    /// Plays one deal in a game.
    let playDeal (playerMap : Map<_, _>) game =

        let rec loop game =

                // take action in the current deal
            let game =
                let infoSet = Game.currentInfoSet game
                let action =
                    match Seq.tryExactlyOne infoSet.LegalActions with
                        | Some action -> action
                        | None -> playerMap[infoSet.Player].Act infoSet
                Game.addAction infoSet.LegalActionType action game

                // deal is over?
            match Game.tryUpdateScore game with
                | Some game -> game
                | None -> loop game

        loop game

    /// Plays the given number of deals.
    let playDeals rng inParallel numDeals playerMap =
        OpenDeal.playDeals rng inParallel numDeals (
            playDeal playerMap)
            |> Seq.reduce (+)

    /// Runs a tournament between two players.
    let run rng inParallel numDeals champion challenger =
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
            playDeals rng inParallel numDeals playerMap
        let payoff =
            (ZeroSum.getPayoff score)[int challengerSeat]
                / float32 numDeals
        score, payoff
