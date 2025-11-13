namespace Hearts.Cfr

open System
open MathNet.Numerics.LinearAlgebra
open Hearts

/// An information set is a set of nodes in a game tree that are
/// indistinguishable for a given player. This type gathers regrets
/// and strategies for an information set.
type InfoSetAcc =
    {
        /// Sum of regrets accumulated so far by this info set.
        RegretSum : Vector<float>

        /// Sum of strategies accumulated so far by this info set.
        StrategySum : Vector<float>
    }

    /// Combines the given information sets.
    static member (+)(a, b) =
        {
            RegretSum = a.RegretSum + b.RegretSum
            StrategySum = a.StrategySum + b.StrategySum
        }

module InfoSetAcc =

    /// Creates an information set accumulator.
    let create regretSum strategySum =
        {
            RegretSum = regretSum
            StrategySum = strategySum
        }

    /// Initial info set accumulator.
    let zero numActions =
        let zero = DenseVector.zero numActions
        create zero zero

    /// Uniform strategy: All actions have equal probability.
    let private uniformStrategy numActions =
        DenseVector.create
            numActions
            (1.0 / float numActions)

    /// Normalizes a strategy such that its elements sum to
    /// 1.0 (to represent action probabilities).
    let private normalize strategy =

            // assume no negative values during normalization
        assert(Vector.forall (fun x -> x >= 0.0) strategy)

        let sum = Vector.sum strategy
        if sum > 0.0 then strategy / sum
        else uniformStrategy strategy.Count

    /// Computes regret-matching strategy from accumulated
    /// regrets.
    let getStrategy infoSetAcc =
        infoSetAcc.RegretSum
            |> Vector.map (max 0.0)   // clamp negative regrets
            |> normalize

    /// Computes average strategy from accumulated strateges.
    let getAverageStrategy infoSetAcc =
        normalize infoSetAcc.StrategySum

module Trainer =

    /// Obtains an accumulator for the given info set.
    let private getAccumulator (infoSet : InformationSet) accumMap =
        let nActions = infoSet.LegalActions.Length
        match Map.tryFind infoSet accumMap with
            | Some accum ->
                assert(accum.RegretSum.Count = nActions)
                assert(accum.StrategySum.Count = nActions)
                accum
            | None ->
                InfoSetAcc.zero nActions   // first visit

    /// Updates the active player's reach probability to reflect
    /// the probability of an action.
    let private updateReachProbabilities
        reachProbs (activePlayer : Seat) actionProb =
        reachProbs
            |> Vector.mapi (fun i x ->
                if i = int activePlayer then
                    x * actionProb
                else x)

    /// Evaluates the utility of the given deal via counterfactual
    /// regret minimization.
    let private cfr accumMap (deal : OpenDeal) =

        /// Top-level CFR loop.
        let rec loop reachProbs deal =
            match ZeroSum.tryGetPayoff deal with
                | Some payoff ->
                    payoff, Array.empty
                | None ->
                    loopNonTerminal reachProbs deal

        /// Recurses for non-terminal game state.
        and loopNonTerminal reachProbs deal =

                // get current player's strategy for this info set
            let infoSet = OpenDeal.currentInfoSet deal
            let accum = getAccumulator infoSet accumMap
            let strategy = InfoSetAcc.getStrategy accum

                // get utility of each action
            let actionUtilities, keyedAccums =
                let utilities, keyedAccumArrays =
                    (infoSet.LegalActions, strategy.ToArray())
                        ||> Array.map2 (fun action actionProb ->
                            let reachProbs =
                                updateReachProbabilities
                                    reachProbs
                                    infoSet.Player
                                    actionProb
                            let deal =
                                OpenDeal.addAction
                                    infoSet.LegalActionType action deal
                            loop reachProbs deal)
                        |> Array.unzip
                utilities, Array.concat keyedAccumArrays

                // utility of this info set is action utilities weighted by action probabilities
            let utility = actionUtilities * strategy

                // accumulate updated regrets and strategy
            let keyedInfoSetAccs =
                let infoSetAcc =
                    let regrets =
                        let opponent =
                            (activePlayer + 1) % LeducHoldem.numPlayers
                        reachProbs[opponent] * (actionUtilities - utility)
                    let strategy =
                        reachProbs[activePlayer] * strategy
                    InfoSetAcc.create regrets strategy
                [|
                    yield! keyedInfoSetAccs
                    yield infoSetKey, infoSetAcc
                |]

            utility, keyedInfoSetAccs

        [| 1.0; 1.0 |]
            |> DenseVector.ofArray
            |> loop ""

    /// Trains for the given number of iterations.
    let train numIterations =

        let utilities, infoSetAccMap =

                // each iteration evaluates a chunk of deals
            let dealChunks =
                let permutations =
                    LeducHoldem.deck
                        |> List.permutations
                        |> Seq.map (fun deck ->
                            Seq.toArray deck[0..1], deck[2])
                        |> Seq.toArray
                let chunkSize = 250
                seq {
                    for i = 0 to numIterations - 1 do
                        yield permutations[i % permutations.Length]
                } |> Seq.chunkBySize chunkSize

                // start with no known info sets
            (Map.empty, dealChunks)
                ||> Seq.mapFold (fun infoSetAccMap deals ->

                        // evaluate each deal in the given chunk
                    let utilities, updateChunks =
                        deals
                            |> Array.Parallel.map
                                (fun (playerCards, communityCard) ->
                                    cfr infoSetAccMap playerCards communityCard)
                            |> Array.unzip

                        // update info sets
                    let infoSetAccMap =
                        seq {
                            yield! Map.toSeq infoSetAccMap
                            for updates in updateChunks do
                                yield! updates
                        }
                            |> Seq.groupBy fst
                            |> Seq.map (fun (key, group) ->
                                let sum =
                                    group
                                        |> Seq.map snd
                                        |> Seq.reduce (+)   // accumulate regrets and strategies
                                key, sum)
                            |> Map

                    Seq.sum utilities, infoSetAccMap)

            // compute average utility per deal
        let utility =
            Seq.sum utilities / float numIterations
        utility, infoSetAccMap

module Program =

    let run () =

            // train
        let numIterations = 50000
        printfn $"Running Leduc Hold'em parallel CFR for {numIterations} iterations"
        printfn $"Server garbage collection: {Runtime.GCSettings.IsServerGC}\n"
        let util, infoSetAccMap = Trainer.train numIterations

            // expected overall utility
        printfn $"Average game value for first player: %0.5f{util}\n"

            // strategy
        printfn "Strategy:"
        for (KeyValue(key, infoSetAcc)) in infoSetAccMap do
            let actions =
                key
                    |> Seq.where Char.IsLower
                    |> Seq.toArray
                    |> String
                    |> LeducHoldem.getLegalActions
            let str =
                let strategy =
                    InfoSetAcc.getAverageStrategy infoSetAcc
                (strategy.ToArray(), actions)
                    ||> Array.map2 (fun prob action ->
                        sprintf "%s: %0.5f" action prob)
                    |> String.concat ", "
            printfn $"%-11s{key}:    {str}"

    let timer = Diagnostics.Stopwatch.StartNew()
    run ()
    printfn ""
    printfn $"Elapsed time: {timer}"
