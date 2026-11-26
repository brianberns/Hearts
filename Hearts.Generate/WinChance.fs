namespace Hearts.Generate

open Hearts

/// Each seat's chance of winning the game from the start of a
/// deal, given the game score: the next deal's outcomes exactly
/// (GameScore's deal model), then a closed form fitted to the
/// exact W by Hearts.Experiments' wcurve. Validation loss on the
/// i005 games 1.03800 (exact W 1.03765, wfit's network 1.0397).
/// Written by Claude.
module WinChance =

    /// Closed form's constants, from wcurve.
    let private a, b, c = 0.5423, 13.8896, 0.5307

    /// Closed form: a softmax of -a s / (b + D)^c, where D is how
    /// far the leader is from 100. Adds weight times it to acc.
    let private addClosedForm (points : int[]) weight (acc : float[]) =
        let low = Array.min points
        let high = Array.max points
        let scale =
            (b + float (max 0 (GameScore.endThreshold - high))) ** c
        let e =
            points |> Array.map (fun p ->
                exp (-a * float (p - low) / scale))
        let total = Array.sum e
        for i in 0 .. Seat.numSeats - 1 do
            acc[i] <- acc[i] + weight * e[i] / total

    /// Each seat's chance of winning the game, by seat.
    let getWinChances rules (gameScore : Score) =
        assert(rules.MoonShotScoring = MoonShotScoringRule.AlwaysAddToOthers)
        assert(rules.TieGame = TieGameRule.KeepPlaying)
        let winners = GameScore.findGameWinners rules.TieGame gameScore
        if winners.IsEmpty then
            let acc = Array.zeroCreate Seat.numSeats
            let after = Array.zeroCreate Seat.numSeats   // reused for speed
            for (dealPoints, prob) in GameScore.dealOutcomes do
                for i in 0 .. Seat.numSeats - 1 do
                    after[i] <- gameScore.Points[i] + dealPoints[i]
                let low = Array.min after
                if Array.max after >= GameScore.endThreshold
                    && (after |> Array.filter ((=) low) |> Array.length) = 1 then
                    let iWinner = Array.findIndex ((=) low) after   // game over
                    acc[iWinner] <- acc[iWinner] + prob
                else
                    addClosedForm after prob acc   // play continues
            Array.map float32 acc
        else
            let share = 1.0f / float32 winners.Count
            Seat.allSeats
                |> Array.map (fun seat ->
                    if winners.Contains(seat) then share
                    else 0.0f)
