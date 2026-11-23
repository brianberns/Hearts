namespace Hearts

module GameScore =

    /// Finds leaders in the given game score.
    let findGameLeaders gameScore =
        let minPoints = Seq.min gameScore.Points
        Score.indexed gameScore
            |> Seq.where (snd >> (=) minPoints)
            |> Seq.map fst
            |> set

    /// End of game point threshold.
    let endThreshold = 100

    /// Finds game winners, if any, in the given game score.
    let findGameWinners tieGameRule gameScore =

            // is at least one player over the threshold?
        let thresholdMet =
            gameScore.Points
                |> Seq.exists (fun points ->
                    points >= endThreshold)

        if thresholdMet then

                // find low scorers
            let leaders = findGameLeaders gameScore
            assert(not leaders.IsEmpty)

                // game is over?
            match tieGameRule with
                | MultipleWinners -> leaders
                | KeepPlaying ->
                    if leaders.Count = 1 then leaders
                    else Set.empty

        else Set.empty

    /// Does the given game score end the game?
    let isOver tieGameRule gameScore =
        findGameWinners tieGameRule gameScore
            |> Set.isEmpty
            |> not
