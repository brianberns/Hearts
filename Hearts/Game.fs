namespace Hearts

open PlayingCards

/// A game of Hearts.
type Game =
    {
        /// Current deal.
        Deal : OpenDeal

        /// Game score at the start of the current deal. Not updated
        /// during play.
        Score : Score
    }

module Game =

    /// Creates a game.
    let create deal score =
        {
            Deal = deal
            Score = score
        }

    /// Creates a new deal.
    let createDeal rng dealer dir =
        Deck.shuffle rng
            |> OpenDeal.fromDeck dealer dir

    /// Creates a new game.
    let createNew rng dealer =
        let deal =
            createDeal rng dealer ExchangeDirection.Left   // games always start with Left pass
        create deal Score.zero

    /// Answers the current player's information set.
    let currentInfoSet game =
        let deal = game.Deal
        let player = OpenDeal.currentPlayer deal
        let hand = deal.UnplayedCardMap[player]
        let outOpt, inOpt =
            deal.ExchangeOpt
                |> Option.map (
                    Exchange.getPassOpts
                        player
                        deal.ClosedDeal.ExchangeDirection)
                |> Option.defaultValue (None, None)
        InformationSet.create
            player hand outOpt inOpt deal.ClosedDeal game.Score

    /// Passes the given card in the given game.
    let addPass card game =
        let deal = OpenDeal.addPass card game.Deal
        { game with Deal = deal }

    /// Plays the given card in the given game. Score of the game
    /// is not updated, even if points are taken.
    let addPlay card game =
        let deal = OpenDeal.addPlay card game.Deal
        { game with Deal = deal }

    /// Takes the given action in the given game's current deal.
    /// Score of the game is not updated, even if points are taken.
    let addAction actionType action game =
        let deal =
            OpenDeal.addAction actionType action game.Deal
        { game with Deal = deal }

    /// End-of-game point threshold.
    let endThreshold = 100

    /// Is a game with the given score over?
    let isOver gameScore =
        gameScore.Points
            |> Seq.exists (fun points ->
                points >= endThreshold)

    /// Finds leaders in the given game.
    let private findGameLeaders game =
        let minPoints = Seq.min game.Score.Points
        Score.indexed game.Score
            |> Seq.where (snd >> (=) minPoints)
            |> Seq.map fst
            |> set

    /// Finds game winners, if any, in the given game.
    let findGameWinners game =
        if isOver game.Score then
            findGameLeaders game
        else Set.empty

    /// Adds the given deal score to the given game.
    let private addDealScore dealScore (game : Game) =
        { game with Score = game.Score + dealScore }

    /// Applies the shoot reward to the given game for the
    /// given shooter.
    let private applyShootReward game shooter =

            // add points to non-shooters
        let game =
            let dealScore =
                Enum.getValues<Seat>
                    |> Array.map (fun seat ->
                        if seat = shooter then 0
                        else ClosedDeal.numPointsPerDeal)
                    |> Score.ofPoints
            addDealScore dealScore game

            // subtract points from shooter instead?
        let leaders = findGameLeaders game
        if leaders.Contains(shooter) then game
        else
            let dealScore =
                Enum.getValues<Seat>
                    |> Array.map (fun seat ->
                        if seat = shooter then
                            -ClosedDeal.numPointsPerDeal
                        else 0)
                    |> Score.ofPoints
            addDealScore dealScore game

    /// Updates the given game's score, including shoot reward, if
    /// the final score of the current deal is inevitable.
    let tryUpdateScore game =
        option {
            let! inevitable = OpenDeal.tryFindInevitable game.Deal
            let dealScore = game.Deal.ClosedDeal.Score + inevitable
            assert(Score.sum dealScore = ClosedDeal.numPointsPerDeal)
            let game =
                match ClosedDeal.tryFindScoreShooter dealScore with
                    | Some shooter -> applyShootReward game shooter
                    | None -> addDealScore dealScore game
            return game
        }

    /// Plays one deal in a game.
    let private playDeal (playerMap : Map<_, _>) game =

        let rec loop game =

                // take action in the current deal
            let game =
                let infoSet = currentInfoSet game
                let action =
                    match Seq.tryExactlyOne infoSet.LegalActions with
                        | Some action -> action
                        | None -> playerMap[infoSet.Player].Act infoSet
                addAction infoSet.LegalActionType action game

                // deal is over?
            match tryUpdateScore game with
                | Some game -> game
                | None -> loop game

        loop game

    /// Plays the given game to completion.
    let playGame rng playerMap game =

        let rec loop game =
            assert(isOver game.Score |> not)

                // play current deal to completion
            let game = playDeal playerMap game

                // stop if game is over
            if isOver game.Score then game
            else
                    // create and play another deal
                let deal =
                    let dealer =
                        Seat.incr 1 game.Deal.ClosedDeal.Dealer
                    let dir =
                        ExchangeDirection.next
                            game.Deal.ClosedDeal.ExchangeDirection
                    createDeal rng dealer dir
                loop { game with Deal = deal }

        loop game

    /// Generates an infinite sequence of games.
    let generate rng =
        Seq.initInfinite (fun iGame ->
            iGame % Seat.numSeats
                |> enum<Seat>
                |> createNew rng)

    /// Plays the given number of games.
    let playGames rng inParallel numGames playFun =
        let map =
            if inParallel then Array.Parallel.map
            else Array.map
        generate rng
            |> Seq.take numGames
            |> Seq.toArray
            |> map playFun

    /// Gets per-player's payoffs for the given game score.
    let getPayoffs gameScore =

            // find high and low scores
        let low = Array.min gameScore.Points
        let high = Array.max gameScore.Points
   
            // game is over?
        if high >= endThreshold then
            gameScore.Points
                |> Array.map (fun x ->
                    if x = low then 1.0f else 0.0f)

        else
                // calculate Softmax terms relative to the low score
            let temperature =
                let volatility = 10.0f   // prevent temperature from dropping to near-zero
                float32 (endThreshold - high) + volatility
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
    let tryGetPayoffs game =
        tryUpdateScore game
            |> Option.map (_.Score >> getPayoffs)
