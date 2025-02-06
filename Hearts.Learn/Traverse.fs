namespace Hearts.Learn

open MathNet.Numerics.LinearAlgebra

open PlayingCards
open Hearts

/// Model Hearts as a zero-sum game.
module ZeroSum =

    /// Gets the payoff for the given deal score from each
    /// player's point of view.
    let getPayoff score =
        let points = score.ScoreMap.Values
        assert(points.Count = Seat.numSeats)
        let sum = Seq.sum points
        let payoff =
            [|
                for pt in points do
                    let otherAvg =
                        float32 (sum - pt)
                            / float32 (Seat.numSeats - 1)
                    otherAvg - float32 pt
            |]
        assert(Seq.sum payoff = 0.0f)
        payoff

    /// Computes the payoff for the given deal, if it is
    /// complete.
    let tryGetPayoff deal =
        Game.tryUpdateScore deal Score.zero
            |> Option.map getPayoff

module Strategy =

    /// Computes strategy from the given per-action regrets.
    /// A strategy is normalized so that its elements sum
    /// to 1.0 (to represent action probabilities).
    let private matchRegrets regrets =

            // find highest-value action
        let idx = Vector.maxIndex regrets

            // scale if possible, or choose highest-value action
        if regrets[idx] > 0.0f then
            let clamped = Vector.map (max 0.0f) regrets   // clamp negative regrets
            clamped / Vector.sum clamped
        else
            DenseVector.init regrets.Count (fun i ->
                if i = idx then 1.0f
                else 0.0f)

    /// Converts a wide vector (indexed by entire deck) to
    /// a narrow vector (indexed by legal plays).
    let toNarrow (legalPlays : _[]) (wide : Vector<_>) =
        assert(wide.Count = Card.numCards)
        legalPlays
            |> Seq.map (
                Card.toIndex >> Vector.get wide)
            |> DenseVector.ofSeq

    /// Converts a narrow vector (indexed by legal plays) to
    /// a wide vector (indexed by entire deck).
    let toWide (legalPlays : _[]) (narrow : Vector<_>) =
        assert(narrow.Count = legalPlays.Length)
        Seq.zip legalPlays narrow
            |> Encoding.encodeCardValues
            |> DenseVector.ofArray

    /// Computes strategy for the given info set (hand + deal)
    /// using the given advantage model.
    let getFromAdvantage (model : AdvantageModel) hand deal legalPlays =
        (AdvantageModel.getAdvantage hand deal model)
            .data<float32>()
            |> DenseVector.ofSeq
            |> toNarrow legalPlays
            |> matchRegrets

module Traverse =

    /// Appends an item to the end of an array.
    let private append items item =
        [| yield! items; yield item |]

    /// Evaluates the utility of the given deal.
    let traverse deal model =

        /// Top-level loop.
        let rec loop deal =
            match ZeroSum.tryGetPayoff deal with
                | Some payoff ->
                    payoff, Array.empty   // deal is over
                | None ->
                    loopNonTerminal deal

        /// Recurses for non-terminal game state.
        and loopNonTerminal deal =
            let hand = OpenDeal.currentHand deal
            let legalPlays =
                deal.ClosedDeal
                    |> ClosedDeal.legalPlays hand
                    |> Seq.toArray
            if legalPlays.Length = 1 then
                addLoop deal legalPlays[0]   // forced play
            else
                    // get utility of current player's strategy
                let player = OpenDeal.currentPlayer deal
                let strategy =
                    Strategy.getFromAdvantage
                        model hand deal.ClosedDeal legalPlays
                let rnd =
                    lock settings.Random
                        settings.Random.NextDouble
                if rnd <= settings.BranchRate then
                    getFullUtility hand player deal legalPlays strategy
                else
                    getOneUtility deal legalPlays strategy

        /// Adds the given play to the given deal and loops.
        and addLoop deal play =
            deal
                |> OpenDeal.addPlay play
                |> loop

        /// Gets the full utility of the given info set (hand + deal).
        and getFullUtility hand player deal legalPlays strategy =

                // get utility of each action
            let actionUtilities, samples =
                let utilityArrays, sampleArrays =
                    legalPlays
                        |> Array.map (addLoop deal)
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
                    hand deal.ClosedDeal wideRegrets
                    |> append samples
            utility.ToArray(), samples

        /// Gets the utility of the given info set (hand + deal)
        /// by sampling a single action.
        and getOneUtility deal legalPlays strategy =
            lock settings.Random (fun () ->
                Vector.sample settings.Random strategy)
                |> Array.get legalPlays
                |> addLoop deal

        loop deal |> snd
