namespace Hearts.Generate

open Hearts

/// Models Hearts as a zero-sum, deal-level game. Game score
/// is ignored.
module DealScore =

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
                    |> Game.tryUpdateScore AlwaysAddToOthers
            return getPayoff game.Score      // game score is now raw deal score
        }
