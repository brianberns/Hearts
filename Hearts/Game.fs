namespace Hearts

open PlayingCards

module Game =

    /// Finds leaders in the given game score.
    let findGameLeaders gameScore =
        let minPoints = Seq.min gameScore.Points
        Score.indexed gameScore
            |> Seq.where (snd >> (=) minPoints)
            |> Seq.map fst
            |> set

    /// End of game point threshold.
    let endThreshold = 100

    /// Does the given game score end the game?
    let isOver gameScore =
        gameScore.Points
            |> Seq.exists (fun points ->
                points >= endThreshold)

    /// Finds game winners, if any, in the given game score.
    let findGameWinners gameScore =
        if isOver gameScore then findGameLeaders gameScore
        else Set.empty
