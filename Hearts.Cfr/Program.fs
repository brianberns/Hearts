namespace Hearts.Cfr

open System
open MathNet.Numerics.LinearAlgebra

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

module LeducCfrTrainer =

    /// Obtains an info set accumulator corresponding to the given key.
    let private getInfoSetAcc infoSetKey infoSetAccMap numActions =
        match Map.tryFind infoSetKey infoSetAccMap with
            | Some infoSetAcc ->
                assert(infoSetAcc.RegretSum.Count = numActions)
                assert(infoSetAcc.StrategySum.Count = numActions)
                infoSetAcc
            | None ->
                InfoSetAcc.zero numActions   // first visit

    /// Updates the active player's reach probability to reflect
    /// the probability of an action.
    let private updateReachProbabilities reachProbs activePlayer actionProb =
        reachProbs
            |> Vector.mapi (fun i x ->
                if i = activePlayer then
                    x * actionProb
                else x)

    /// Negates opponent's utilties (assuming a zero-zum game).
    let private getActiveUtilities utilities =
        utilities
            |> Seq.map (~-)
            |> DenseVector.ofSeq

    /// Evaluates the utility of the given deal via counterfactual
    /// regret minimization.
    let private cfr infoSetAccMap playerCards communityCard =

        /// Top-level CFR loop.
        let rec loop (history : string) reachProbs =
            let rounds = history.Split('d')

                // game is over?
            if LeducHoldem.isTerminal rounds then
                let payoff =
                    LeducHoldem.getPayoff
                        playerCards
                        communityCard
                        rounds
                float payoff, Array.empty

                // first round is over?
            elif LeducHoldem.isRoundEnd (Array.last rounds) then
                let sign =
                    match history with
                        | "xbc" | "brc" -> -1.0
                        | _ -> 1.0   // active player to play again
                let utility, keyedInfoSetAccs =
                    loop (history + "d") reachProbs
                sign * utility, keyedInfoSetAccs

                // player action
            else
                let activePlayer =
                    (Array.last rounds).Length
                        % LeducHoldem.numPlayers
                let infoSetKey =
                    sprintf "%s%s %s"
                        playerCards[activePlayer]
                        (if rounds.Length = 2 then communityCard
                         else "")
                        history
                loopNonTerminal
                    history
                    activePlayer
                    infoSetKey
                    reachProbs

        /// Recurses for non-terminal game state.
        and loopNonTerminal
            history
            activePlayer
            infoSetKey
            reachProbs =

                // get info set for current state from this player's point of view
            let actions = LeducHoldem.getLegalActions history
            let infoSetAcc =
                getInfoSetAcc infoSetKey infoSetAccMap actions.Length

                // get player's current strategy for this info set
            let strategy = InfoSetAcc.getStrategy infoSetAcc

                // get utility of each action
            let actionUtilities, keyedInfoSetAccs =
                let utilities, keyedInfoSetAccArrays =
                    (actions, strategy.ToArray())
                        ||> Array.map2 (fun action actionProb ->
                            let reachProbs =
                                updateReachProbabilities
                                    reachProbs
                                    activePlayer
                                    actionProb
                            loop (history + action) reachProbs)
                        |> Array.unzip
                getActiveUtilities utilities,
                Array.concat keyedInfoSetAccArrays

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
        let util, infoSetAccMap = LeducCfrTrainer.train numIterations

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
