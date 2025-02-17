namespace Hearts.Learn

open MathNet.Numerics.LinearAlgebra

open Hearts
open Hearts.Model

/// Model Hearts as a zero-sum game.
module ZeroSum =

    /// Gets the payoff for the given deal score from each
    /// player's point of view.
    let getPayoff score =
        let points = score.ScoreMap.Values
        assert(points.Count = Seat.numSeats)
        let sum = Seq.sum points
        [|
            for pt in points do
                let otherAvg =
                    float32 (sum - pt)
                        / float32 (Seat.numSeats - 1)
                otherAvg - float32 pt
        |]

    /// Computes the payoff for the given deal, if it is
    /// complete.
    let tryGetPayoff deal =
        Game.tryUpdateScore deal Score.zero
            |> Option.map getPayoff

module Traverse =

    /// Appends an item to the end of an array.
    let private append items item =
        [| yield! items; yield item |]

    /// Evaluates the utility of the given deal.
    let traverse iter deal model =

        /// Top-level loop.
        let rec loop deal depth =
            assert(depth
                = ClosedDeal.numCardsPlayed deal.ClosedDeal)
            match ZeroSum.tryGetPayoff deal with
                | Some payoff ->
                    payoff, Array.empty   // deal is over
                | None ->
                    loopNonTerminal deal depth

        /// Recurses for non-terminal game state.
        and loopNonTerminal deal depth =
            let hand = OpenDeal.currentHand deal
            let legalPlays =
                deal.ClosedDeal
                    |> ClosedDeal.legalPlays hand
                    |> Seq.toArray
            if legalPlays.Length = 1 then
                addLoop deal depth legalPlays[0]   // forced play
            else
                    // get utility of current player's strategy
                let player = OpenDeal.currentPlayer deal
                let strategy =
                    Strategy.getFromAdvantage
                        model hand deal.ClosedDeal legalPlays
                let rnd =
                    lock settings.Random
                        settings.Random.NextDouble
                let threshold =
                    settings.SampleDecaySpeed
                        / (settings.SampleDecaySpeed + float depth)
                if rnd <= threshold then
                    getFullUtility
                        hand player deal depth legalPlays strategy
                else
                    getOneUtility deal depth legalPlays strategy

        /// Adds the given play to the given deal and loops.
        and addLoop deal depth play =
            let deal = OpenDeal.addPlay play deal
            loop deal (depth + 1)

        /// Gets the full utility of the given info set (hand + deal).
        and getFullUtility hand player deal depth legalPlays strategy =

                // get utility of each action
            let actionUtilities, samples =
                let utilityArrays, sampleArrays =
                    legalPlays
                        |> Array.map (addLoop deal depth)
                        |> Array.unzip
                DenseMatrix.ofColumnArrays utilityArrays,
                Array.concat sampleArrays
            assert(actionUtilities.ColumnCount = legalPlays.Length)
            assert(actionUtilities.RowCount = Seat.numSeats)

                // utility of this info set is action utilities weighted by action probabilities
            let utility = actionUtilities * strategy
            assert(utility.Count = Seat.numSeats)
            let samples =
                let wideRegrets =
                    let idx = int player
                    (actionUtilities.Row(idx) - utility[idx])
                        |> Strategy.toWide legalPlays
                AdvantageSample.create
                    hand deal.ClosedDeal wideRegrets iter
                    |> append samples
            utility.ToArray(), samples

        /// Gets the utility of the given info set (hand + deal)
        /// by sampling a single action.
        and getOneUtility deal depth legalPlays strategy =
            lock settings.Random (fun () ->
                Vector.sample settings.Random strategy)
                |> Array.get legalPlays
                |> addLoop deal depth

        loop deal 0 |> snd
