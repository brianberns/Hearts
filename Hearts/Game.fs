namespace Hearts

open System
open PlayingCards

/// A game of Hearts.
type Game =
    {
        /// Random number generator used to generate deals.
        Random : Random

        /// Current deal.
        Deal : OpenDeal

        /// Game score at the start of the current deal. Not updated
        /// during play.
        Score : Score
    }

module Game =

    /// Creates a new deal.
    let private createDeal rng dealer dir =
        Deck.shuffle rng
            |> OpenDeal.fromDeck dealer dir

    /// Creates a new game.
    let create rng dealer =
        let deal =
            createDeal rng dealer ExchangeDirection.Left   // games always start with Left pass
        {
            Random = rng
            Deal = deal
            Score = Score.zero
        }

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

    /// Takes the given action in the given game's current deal.
    /// Score of the game is not updated, even if points are taken.
    let addAction actionType action game =
        let deal =
            OpenDeal.addAction actionType action game.Deal
        { game with Deal = deal }

    /// End-of-game point threshold.
    let endThreshold = 100

    /// Is the given game over?
    let isOver game =
        game.Score.Points
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
        if isOver game then findGameLeaders game
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
    let playGame playerMap game =

        let rec loop game =
            assert(isOver game |> not)

                // play current deal to completion
            let game = playDeal playerMap game

                // stop if game is over
            if isOver game then game
            else
                    // create and play another deal
                let deal =
                    let dealer =
                        Seat.incr 1 game.Deal.ClosedDeal.Dealer
                    let dir =
                        ExchangeDirection.next
                            game.Deal.ClosedDeal.ExchangeDirection
                    createDeal game.Random dealer dir
                loop { game with Deal = deal }

        loop game

    /// Generates an infinite sequence of games.
    let generate rng =
        Seq.initInfinite (fun iGame ->
            iGame % Seat.numSeats
                |> enum<Seat>
                |> create rng)

    /// Plays the given number of games.
    let playGames rng inParallel numGames playFun =
        let map =
            if inParallel then Array.Parallel.map
            else Array.map
        generate rng
            |> Seq.take numGames
            |> Seq.toArray
            |> map playFun
