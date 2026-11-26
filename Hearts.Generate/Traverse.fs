namespace Hearts.Generate

open System
open System.Collections.Concurrent

open MathNet.Numerics.LinearAlgebra

open Hearts
open Hearts.Learn
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

    /// Memoizes the given function.
    let private memoize (f : 'key -> 'value) =
        let cache = ConcurrentDictionary<'key, 'value>()
        fun key -> cache.GetOrAdd(key, f)

    /// Evaluates the utility of the current deal within the
    /// given game.
    let traverse settings state (rng : Random) (game : Game) =
        assert(
            state.WinProbabilityModelOpt.IsSome
                = settings.GameLevelPayoff)

            // cache win chances by game score for speed
        let getWinChances =
            match state.WinProbabilityModelOpt with
                | Some winProbModel ->
                    memoize (
                        WinProbability.getWinChances
                            settings.Rules winProbModel)
                | None -> fun _ -> Array.empty   // not used

            // each player's win probability at the start of the deal
        let beforeProbs = getWinChances game.Score

        /// Computes player payoffs for the given game's deal, if
        /// they are inevitable. Game-level payoff is the change
        /// in a player's chance of winning the game, from the game
        /// score before the deal to the game score after it.
        /// Payoffs sum to zero.
        let tryGetGamePayoff game =
            option {
                let! game =
                    Game.tryUpdateScore
                        settings.Rules.MoonShotScoring game
                let afterProbs = getWinChances game.Score
                return Array.map2 (-) afterProbs beforeProbs
            }

        /// Game- or deal-level payoffs.
        let tryGetPayoff game =
            if settings.GameLevelPayoff then
                tryGetGamePayoff game
            else
                DealScore.tryGetPayoff game.Deal

        /// Top-level loop.
        let rec loop game depth =
            match tryGetPayoff game with
                | Some payoff ->
                    Node.complete   // deal is over
                        payoff
                        None
                        Array.empty
                | None ->
                    loopNonTerminal game depth

        /// Recurses for non-terminal game state.
        and loopNonTerminal game depth =
            let infoSet = Game.currentInfoSet settings.Rules game
            let legalActions = infoSet.LegalActions
            if legalActions.Length = 1 then
                addLoop game depth
                    infoSet.LegalActionType legalActions[0]           // forced action
            else
                    // get utility of current player's strategy
                let rnd = lock rng (fun () -> rng.NextDouble())       // lock RNG to allow multi-threaded batch inference
                let threshold =
                    settings.SampleBranchRate
                        / (settings.SampleBranchRate + float depth)   // nodes near the root have a greater chance of being expanded
                let getUtility =
                    if rnd <= threshold then getFullUtility
                    else getOneUtility
                let cont = getUtility infoSet game depth
                Node.getStrategy infoSet cont

        /// Adds the given action to the given game's deal and loops.
        and addLoop game depth actionType action =
            let deal =
                OpenDeal.addAction
                    settings.Rules actionType action game.Deal
            let game = { game with Deal = deal }
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
                    let encoding = Encoding.encode infoSet
                    let wideRegrets =
                        let idx = int infoSet.Player
                        (actionUtilities.Row(idx) - utility[idx])
                            |> Strategy.toWide legalActions
                    AdvantageSample.create
                        encoding wideRegrets state.SampleStore.Iteration
                Node.complete
                    (utility.ToArray())
                    (Some sample)
                    children

            Node.getUtility infoSet results cont

        /// Gets the utility of the given info set by
        /// sampling a single action.
        and getOneUtility infoSet game depth strategy =
            let result =
                lock rng (fun () -> Vector.sample rng strategy)   // lock RNG to allow multi-threaded batch inference
                    |> Array.get infoSet.LegalActions
                    |> addLoop game (depth+1) infoSet.LegalActionType
            Node.getUtility
                infoSet
                [| result |]
                (Array.exactlyOne >> Complete)

        let game =
            if settings.GameLevelPayoff then game
            else { game with Score = Score.zero }   // ignore incoming game score
        loop game 0
