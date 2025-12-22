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

    /// Answers the current player's information set.
    let currentInfoSet game =
        OpenDeal.currentInfoSet game.Deal game.Score

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
        { game with
            Score = game.Score + dealScore }

    /// Applies the shoot reward to the given game for the
    /// given shooter.
    let private applyShootReward (game : Game) shooter =

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
