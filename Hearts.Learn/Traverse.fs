namespace Hearts.Learn

open MathNet.Numerics.LinearAlgebra

open PlayingCards
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

    /// Function to get strategy for a given info set.
    type GetStrategy =
        InformationSet
            -> Card[]            // legal actions
            -> Vector<float32>   // per-action strategy

    /// Evaluates the utility of the given deal.
    let traverse iter deal (getStrategy : GetStrategy) =

        /// Top-level loop.
        let rec loop deal depth =
            match ZeroSum.tryGetPayoff deal with
                | Some payoff ->
                    payoff, Array.empty   // deal is over
                | None ->
                    loopNonTerminal deal depth

        /// Recurses for non-terminal game state.
        and loopNonTerminal deal depth =
            let infoSet = OpenDeal.currentInfoSet deal
            let actionType, legalActions =
                InformationSet.legalActions infoSet
            if legalActions.Length = 1 then
                addLoop deal depth actionType legalActions[0]   // forced action
            else
                    // get utility of current player's strategy
                let strategy = getStrategy infoSet legalActions
                let rnd =
                    lock settings.Random (fun () ->
                        settings.Random.NextDouble())
                let threshold =
                    settings.SampleDecay
                        / (settings.SampleDecay + float depth)
                if rnd <= threshold then
                    getFullUtility
                        infoSet deal depth actionType legalActions strategy
                else
                    getOneUtility deal depth actionType legalActions strategy

        /// Adds the given action to the given deal and loops.
        and addLoop deal depth actionType action =
            let deal = OpenDeal.addAction actionType action deal
            loop deal depth

        /// Gets the full utility of the given info set.
        and getFullUtility infoSet deal depth actionType legalActions strategy =

                // get utility of each action
            let actionUtilities, samples =
                let utilityArrays, sampleArrays =
                    legalActions
                        |> Array.map (addLoop deal (depth+1) actionType)
                        |> Array.unzip
                DenseMatrix.ofColumnArrays utilityArrays,
                Array.concat sampleArrays
            assert(actionUtilities.ColumnCount = legalActions.Length)
            assert(actionUtilities.RowCount = Seat.numSeats)

                // utility of this info set is action utilities weighted by action probabilities
            let utility = actionUtilities * strategy
            assert(utility.Count = Seat.numSeats)
            let samples =
                let wideRegrets =
                    let idx = int infoSet.Player
                    (actionUtilities.Row(idx) - utility[idx])
                        |> Strategy.toWide legalActions
                AdvantageSample.create infoSet wideRegrets iter
                    |> append samples
            utility.ToArray(), samples

        /// Gets the utility of the given info set by
        /// sampling a single action.
        and getOneUtility deal depth actionType legalActions strategy =
            lock settings.Random (fun () ->
                Vector.sample settings.Random strategy)
                |> Array.get legalActions
                |> addLoop deal (depth+1) actionType

        loop deal 0 |> snd
