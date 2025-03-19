﻿namespace Hearts.Learn

open System

open MathNet.Numerics.LinearAlgebra

open Hearts
open Hearts.Model

type GetStrategy =
    {
        InformationSet : InformationSet
        Continuation : Vector<float32> (*per-action strategy*) -> Node
    }

and GetUtility =
    {
        InformationSet : InformationSet
        Results : Node[]
        Continuation : Complete[] (*children*) -> Node
    }

and Complete =
    {
        Utilities : float32[]
        SampleOpt : Option<AdvantageSample>
        Children : Complete[]
    }

and Node =
    | GetStrategy of GetStrategy
    | GetUtility of GetUtility
    | Complete of Complete

module Node =

    let getStrategy infoSet cont =
        GetStrategy {
            InformationSet = infoSet
            Continuation = cont
        }

    let getUtility infoSet results cont =
        GetUtility {
            InformationSet = infoSet
            Results = results
            Continuation = cont
        }

    let complete utilities sampleOpt children =
        Complete {
            Utilities = utilities
            SampleOpt = sampleOpt
            Children = children
        }

/// Model Hearts as a zero-sum game.
module private ZeroSum =

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

    /// Evaluates the utility of the given deal.
    let traverse iter deal (rng : Random) =

        /// Top-level loop.
        let rec loop deal depth : Node =
            match ZeroSum.tryGetPayoff deal with
                | Some payoff ->
                    Node.complete   // deal is over
                        payoff
                        None
                        Array.empty
                | None ->
                    loopNonTerminal deal depth

        /// Recurses for non-terminal game state.
        and loopNonTerminal deal depth : Node =
            let infoSet = OpenDeal.currentInfoSet deal
            let legalActions = infoSet.LegalActions
            if legalActions.Length = 1 then
                addLoop deal depth
                    infoSet.LegalActionType legalActions[0]   // forced action
            else
                    // get utility of current player's strategy
                let rnd = rng.NextDouble()
                let threshold =
                    settings.SampleDecay
                        / (settings.SampleDecay + float depth)
                let getUtility =
                    if rnd <= threshold then getFullUtility
                    else getOneUtility
                let cont =
                    getUtility infoSet deal depth
                Node.getStrategy infoSet cont

        /// Adds the given action to the given deal and loops.
        and addLoop deal depth actionType action : Node =
            let deal = OpenDeal.addAction actionType action deal
            loop deal depth

        /// Gets the full utility of the given info set.
        and getFullUtility infoSet deal depth strategy =
            let legalActions = infoSet.LegalActions
            let results =
                legalActions
                    |> Array.map (
                        addLoop deal (depth+1) infoSet.LegalActionType)

            let cont children =

                    // get utility of each action
                let actionUtilities =
                    children
                        |> Array.map _.Utilities
                        |> DenseMatrix.ofColumnArrays
                assert(actionUtilities.ColumnCount = legalActions.Length)
                assert(actionUtilities.RowCount = Seat.numSeats)

                    // utility of this info set is action utilities weighted by action probabilities
                let utility = actionUtilities * strategy
                assert(utility.Count = Seat.numSeats)
                let sample =
                    let wideRegrets =
                        let idx = int infoSet.Player
                        (actionUtilities.Row(idx) - utility[idx])
                            |> Strategy.toWide legalActions
                    AdvantageSample.create infoSet wideRegrets iter
                Node.complete
                    (utility.ToArray())
                    (Some sample)
                    children

            Node.getUtility infoSet results cont

        /// Gets the utility of the given info set by
        /// sampling a single action.
        and getOneUtility infoSet deal depth strategy =
            let result =
                Vector.sample rng strategy
                    |> Array.get infoSet.LegalActions
                    |> addLoop deal (depth+1) infoSet.LegalActionType
            Node.getUtility
                infoSet
                [|result|]
                (Array.exactlyOne >> Complete)

        loop deal 0
