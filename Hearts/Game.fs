namespace Hearts

open PlayingCards

module Game =

    /// Finds leaders in the given game score.
    let private findGameLeaders gameScore =
        let minPoints = Seq.min gameScore.Points
        Score.indexed gameScore
            |> Seq.where (snd >> (=) minPoints)
            |> Seq.map fst
            |> set

    /// End of game point threshold.
    let endThreshold = 100

    /// Finds game winners, if any, in the given game score.
    let findGameWinners gameScore =
        let isOver =
            gameScore.Points
                |> Seq.exists (fun points ->
                    points >= endThreshold)
        if isOver then findGameLeaders gameScore
        else Set.empty

    /// Applies the shoot reward to the given game score for
    /// the given shooter.
    let private applyShootReward gameScore shooter =

            // add points to non-shooters
        let gameScore' =
            let dealScore =
                Enum.getValues<Seat>
                    |> Array.map (fun seat ->
                        if seat = shooter then 0
                        else ClosedDeal.numPointsPerDeal)
                    |> Score.ofPoints
            gameScore + dealScore

            // subtract points from shooter instead?
        let leaders = findGameLeaders gameScore'
        if leaders.Contains(shooter) then
            gameScore'
        else
            let dealScore =
                Enum.getValues<Seat>
                    |> Array.map (fun seat ->
                        if seat = shooter then
                            -ClosedDeal.numPointsPerDeal
                        else 0)
                    |> Score.ofPoints
            gameScore + dealScore

    /// Updates the given game score, including shoot reward, if
    /// the final score of the given deal is inevitable.
    let tryUpdateScore deal gameScore =
        option {
            let! inevitable = OpenDeal.tryFindInevitable deal
            let dealScore = deal.ClosedDeal.Score + inevitable
            assert(Score.sum dealScore = ClosedDeal.numPointsPerDeal)
            return ClosedDeal.tryFindScoreShooter dealScore
                |> Option.map (applyShootReward gameScore)
                |> Option.defaultValue (gameScore + dealScore)
        }
