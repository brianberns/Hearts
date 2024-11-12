namespace Hearts

open PlayingCards

module Game =

    /// End of game point threshold.
    let endThreshold = 40   // 100

    /// Finds game winners, if any, in the given game score.
    let findGameWinners gameScore =
        let isOver =
            gameScore.ScoreMap.Values
                |> Seq.exists (fun points ->
                    points >= endThreshold)
        if isOver then
            let minPoints = Seq.min gameScore.ScoreMap.Values
            gameScore.ScoreMap
                |> Map.toSeq
                |> Seq.where (snd >> (=) minPoints)
                |> Seq.map fst
                |> set
        else Set.empty

    /// Applies the shoot reward to the given game score for
    /// the given shooter.
    let private applyShootReward gameScore shooter =

        let toScore seatPoints =
            { ScoreMap = Map seatPoints }

            // add points to non-shooters
        let gameScore' =
            Enum.getValues<Seat>
                |> Seq.map (fun seat ->
                    let points =
                        if seat = shooter then 0
                        else OpenDeal.numPointsPerDeal
                    seat, points)
                |> toScore

            // subtract points from shooter instead?
        let winners = findGameWinners gameScore
        if winners.Count = 0 || winners.Contains(shooter) then
            gameScore'
        else
            Enum.getValues<Seat>
                |> Seq.map (fun seat ->
                    let points =
                        if seat = shooter then -OpenDeal.numPointsPerDeal
                        else 0
                    seat, points)
                |> toScore

    /// Determines the final score of the deal, including shoot
    /// reward, if possible.
    let tryFinalScore deal gameScore =
        option {
            let! inevitable = OpenDeal.tryFindInevitable deal
            let dealScore = deal.ClosedDeal.Score + inevitable
            assert(Score.sum dealScore = OpenDeal.numPointsPerDeal)
            return OpenDeal.tryFindShooter dealScore
                |> Option.map (applyShootReward gameScore)
                |> Option.defaultValue dealScore
        }
