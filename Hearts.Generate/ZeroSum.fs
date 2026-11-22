namespace Hearts.Generate

open Hearts

/// Model Hearts as a zero-sum game.
module ZeroSum =

    /// Gets the payoff for the given raw deal score from each
    /// player's point of view.
    let getPayoff dealScore =
        let points = dealScore.Points
        assert(points.Length = Seat.numSeats)
        let sum = Seq.sum points
        [|
            for pt in points do
                let otherAvg =
                    float32 (sum - pt)
                        / float32 (Seat.numSeats - 1)
                otherAvg - float32 pt
        |]

    /// Computes the payoff for the given deal, if it is inevitable.
    let tryGetPayoff deal =
        option {
            let! game =
                deal
                    |> Game.createFirst      // as if this is the first deal of a game
                    |> Game.tryUpdateScore
            return getPayoff game.Score      // game score is now raw deal score
        }
