namespace Hearts.Learn

open System

open MathNet.Numerics.LinearAlgebra

open Hearts
open Hearts.Model

/// Initial node state, awaiting strategy.
type GetStrategy =
    {
        /// Information set in this node.
        InformationSet : InformationSet

        /// Leads to a node in one of the other two states.
        Continuation : Vector<float32> (*per-action strategy*) -> Node
    }

/// Node state awaiting complete children.
and GetUtility =
    {
        /// Information set in this node.
        InformationSet : InformationSet

        /// Incomplete child nodes.
        Children : Node[]

        /// Leads to a completed node.
        Continuation : Complete[] (*children*) -> Node
    }

/// Final node state.
and Complete =
    {
        /// Per-player utility of this node.
        Utilities : float32[]

        /// Sample representing this node.
        SampleOpt : Option<AdvantageSample>

        /// Complete children.
        Children : Complete[]
    }

/// Game state.
and Node =

    /// Awaiting strategy.
    | GetStrategy of GetStrategy

    /// Awaiting utility.
    | GetUtility of GetUtility

    /// Final.
    | Complete of Complete

module Node =

    /// Creates a node.
    let getStrategy infoSet cont =
        GetStrategy {
            InformationSet = infoSet
            Continuation = cont
        }

    /// Creates a node.
    let getUtility infoSet children cont =
        GetUtility {
            InformationSet = infoSet
            Children = children
            Continuation = cont
        }

    /// Creates a node.
    let complete utilities sampleOpt children =
        Complete {
            Utilities = utilities
            SampleOpt = sampleOpt
            Children = children
        }

module Traverse =

    /// Evaluates the utility of the given game.
    let traverse iter game (rng : Random) =

        /// Top-level loop.
        let rec loop game depth =
            match Game.tryGetPayoffs game with
                | Some payoffs ->
                    Node.complete payoffs None Array.empty   // current deal is over
                | None ->
                    loopNonTerminal game depth

        /// Recurses for non-terminal game state. Current deal
        /// is not over.
        and loopNonTerminal game depth =
            assert(not (ClosedDeal.isComplete game.Deal.ClosedDeal))
            let infoSet = Game.currentInfoSet game
            if infoSet.LegalActions.Length = 1 then
                addLoop game depth
                    infoSet.LegalActionType infoSet.LegalActions[0]   // forced action
            else
                    // get utility of current player's strategy
                let rnd = lock rng (fun () -> rng.NextDouble())
                let threshold =
                    settings.SampleBranchRate
                        / (settings.SampleBranchRate + float depth)   // nodes near the root have a higher chance of being expanded
                let getUtility =
                    if rnd <= threshold then getFullUtility
                    else getOneUtility
                let cont = getUtility infoSet game depth
                Node.getStrategy infoSet cont

        /// Adds the given action to the given deal and loops.
        and addLoop game depth actionType action =
            let game = Game.addAction actionType action game
            loop game depth

        /// Gets the full utility of the given info set.
        and getFullUtility infoSet game depth strategy =
            let legalActions = infoSet.LegalActions
            let results =
                legalActions
                    |> Array.map (
                        addLoop game (depth+1) infoSet.LegalActionType)

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
        and getOneUtility infoSet game depth strategy =
            let result =
                lock rng (fun () ->
                    Vector.sample rng strategy)
                    |> Array.get infoSet.LegalActions
                    |> addLoop game (depth+1) infoSet.LegalActionType
            Node.getUtility
                infoSet
                [|result|]
                (Array.exactlyOne >> Complete)

        loop game 0
