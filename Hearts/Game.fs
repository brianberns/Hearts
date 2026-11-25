namespace Hearts

open PlayingCards

/// The state of a game of Hearts. Previous deals in the game
/// are lost.
type Game =
    {
        /// Current deal.
        Deal : OpenDeal

        /// Game score at the start of the current deal. Not updated
        /// during play.
        Score : Score
    }

module Game =

    /// Creates a new game.
    let create deal score =
        {
            Deal = deal
            Score = score
        }

    /// Creates a new game with the given first deal.
    let createFirst deal =
        create deal Score.zero

    /// Answers the current player's information set.
    let currentInfoSet rules game =
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
            rules player hand outOpt inOpt deal.ClosedDeal game.Score

    /// Passes the given card in the given game.
    let addPass card game =
        let deal = OpenDeal.addPass card game.Deal
        { game with Deal = deal }

    /// Plays the given card in the given game. Score of the game
    /// is not updated, even if points are taken.
    let addPlay rules card game =
        let deal = OpenDeal.addPlay rules card game.Deal
        { game with Deal = deal }

    /// Takes the given action in the given game's current deal.
    /// Score of the game is not updated, even if points are taken.
    let addAction rules actionType action game =
        let deal =
            OpenDeal.addAction rules actionType action game.Deal
        { game with Deal = deal }

    /// Adds the given deal score to the given game.
    let private addDealScore dealScore (game : Game) =
        { game with Score = game.Score + dealScore }

    /// Applies the shoot reward to the given game for the
    /// given shooter.
    let private applyShootReward scoringRule shooter game =

            // add points to non-shooters
        let game' =
            let dealScore =
                Seat.allSeats
                    |> Array.map (fun seat ->
                        if seat = shooter then 0
                        else ClosedDeal.numPointsPerDeal)
                    |> Score.ofPoints
            addDealScore dealScore game

            // subtract points from shooter instead?
        if scoringRule = AllowSubtraction then
            let leaders = GameScore.findGameLeaders game'.Score
            if leaders.Contains(shooter) then game'
            else
                let dealScore =
                    Seat.allSeats
                        |> Array.map (fun seat ->
                            if seat = shooter then
                                -ClosedDeal.numPointsPerDeal
                            else 0)
                        |> Score.ofPoints
                addDealScore dealScore game
        else game'

    /// Updates the given game's score, including shoot reward, if
    /// the final score of the current deal is inevitable.
    let tryUpdateScore scoringRule game =
        option {
            let! inevitable = OpenDeal.tryFindInevitable game.Deal
            let dealScore = game.Deal.ClosedDeal.Score + inevitable
            assert(Score.sum dealScore = ClosedDeal.numPointsPerDeal)
            return
                match ClosedDeal.tryFindScoreShooter dealScore with
                    | Some shooter ->
                        applyShootReward scoringRule shooter game
                    | None -> addDealScore dealScore game
        }

    /// Plays one deal in a game.
    let private playDeal rules (playerMap : Map<_, _>) game =

        let rec loop game =

                // take action in the current deal
            let game =
                let infoSet = currentInfoSet rules game
                let action =
                    Player.act infoSet playerMap[infoSet.Player]
                addAction rules infoSet.LegalActionType action game

                // deal is over?
            match tryUpdateScore rules.MoonShotScoring game with
                | Some game -> game
                | None -> loop game

        loop game

    /// Creates a new deal.
    let private createDeal rng dealer dir =
        Deck.shuffle rng
            |> OpenDeal.fromDeck dealer dir

    /// Starts a new game and plays it to completion.
    let playGame rules rng playerMap dealer =

        /// Plays the given game to completion.
        let rec loop game =
            assert(GameScore.isOver rules.TieGame game.Score |> not)

                // play current deal to completion
            let game = playDeal rules playerMap game

                // stop if game is over
            if GameScore.isOver rules.TieGame game.Score then
                game.Score
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

        createDeal rng dealer ExchangeDirection.Left   // games always start with Left pass
            |> createFirst
            |> loop
