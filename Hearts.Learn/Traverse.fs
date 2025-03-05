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
    let traverse iter deal getStrategy =

        /// Top-level loop.
        let rec loop deal depth =
            match ZeroSum.tryGetPayoff deal with
                | Some payoff ->
                    payoff, Array.empty   // deal is over
                | None ->
                    loopNonTerminal deal depth

        /// Recurses for non-terminal game state.
        and loopNonTerminal deal depth =
            let hand = OpenDeal.currentHand deal
            let moveType, legalMoves =
                deal.ClosedDeal
                    |> ClosedDeal.legalMoves
                        hand deal.ExchangeOpt
            if legalMoves.Length = 1 then
                addLoop deal depth moveType legalMoves[0]   // forced move
            else
                    // get utility of current player's strategy
                let player = OpenDeal.currentPlayer deal
                let strategy : Vector<float32> =
                    getStrategy
                        hand deal.ClosedDeal legalMoves
                let rnd =
                    lock settings.Random (fun () ->
                        settings.Random.NextDouble())
                let threshold =
                    settings.SampleDecay
                        / (settings.SampleDecay + float depth)
                if rnd <= threshold then
                    getFullUtility
                        hand player deal depth moveType legalMoves strategy
                else
                    getOneUtility deal depth moveType legalMoves strategy

        /// Adds the given move to the given deal and loops.
        and addLoop deal depth moveType move =
            let deal = OpenDeal.addMove moveType move deal
            loop deal (depth + 1)

        /// Gets the full utility of the given info set (hand + deal).
        and getFullUtility hand player deal depth moveType legalMoves strategy =

                // get utility of each action
            let actionUtilities, samples =
                let utilityArrays, sampleArrays =
                    legalMoves
                        |> Array.map (addLoop deal depth moveType)
                        |> Array.unzip
                DenseMatrix.ofColumnArrays utilityArrays,
                Array.concat sampleArrays
            assert(actionUtilities.ColumnCount = legalMoves.Length)
            assert(actionUtilities.RowCount = Seat.numSeats)

                // utility of this info set is action utilities weighted by action probabilities
            let utility = actionUtilities * strategy
            assert(utility.Count = Seat.numSeats)
            let samples =
                let wideRegrets =
                    let idx = int player
                    (actionUtilities.Row(idx) - utility[idx])
                        |> Strategy.toWide legalMoves
                AdvantageSample.create
                    hand deal.ClosedDeal wideRegrets iter
                    |> append samples
            utility.ToArray(), samples

        /// Gets the utility of the given info set (hand + deal)
        /// by sampling a single action.
        and getOneUtility deal depth moveType legalMoves strategy =
            lock settings.Random (fun () ->
                Vector.sample settings.Random strategy)
                |> Array.get legalMoves
                |> addLoop deal depth moveType

        loop deal 0 |> snd
