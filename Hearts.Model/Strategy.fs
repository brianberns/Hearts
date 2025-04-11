namespace Hearts.Model

open MathNet.Numerics.LinearAlgebra

open PlayingCards
open Hearts

module Strategy =

    /// Creates a random strategy of the given length.
    let random n =
        DenseVector.create n (1.0f / float32 n)

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
    /// a narrow vector (indexed by legal actions).
    let private toNarrow (legalActions : _[]) (wide : Vector<_>) =
        assert(wide.Count = Card.numCards)
        legalActions
            |> Seq.map (
                Card.toIndex >> Vector.get wide)
            |> DenseVector.ofSeq

    /// Converts a narrow vector (indexed by legal actions) to
    /// a wide vector (indexed by entire deck).
    let toWide (legalActions : _[]) (narrow : Vector<float32>) =
        assert(narrow.Count = legalActions.Length)
        Seq.zip legalActions narrow
            |> Encoding.encodeCardValues
            |> DenseVector.ofArray

    /// Computes strategies for the given info sets using
    /// the given model.
    let private getFromAdvantage model infoSets encode =

        if Array.length infoSets > 0 then

                // run model on GPU
            use advantages =
                AdvantageModel.getAdvantages
                    infoSets encode model
            assert(advantages.shape[0] = infoSets.Length)

                // access data on CPU
            let nCols = int advantages.shape[1]
            assert(nCols = Card.numCards)
            let data =
                use accessor = advantages.data<float32>()
                accessor.ToArray()
            [|
                for iRow, infoSet in Seq.indexed infoSets do
                    let iStart = iRow * nCols
                    data[iStart .. iStart + nCols - 1]
                        |> DenseVector.ofSeq
                        |> toNarrow infoSet.LegalActions
                        |> matchRegrets
            |]

        else Array.empty

    /// Computes strategies for the given info sets using
    /// the given model.
    let getFromModel model infoSets =

            // partition info sets
        let passPairs, playPairs =
            infoSets
                |> Array.indexed
                |> Array.partition (fun (_, infoSet) ->
                    infoSet.LegalActionType = ActionType.Pass)
        let passIdxs, passInfoSets = Array.unzip passPairs
        let playIdxs, playInfoSets = Array.unzip playPairs

            // get strategies for each partition
        let passStrats =
            getFromAdvantage
                model.ExchangeModel
                passInfoSets
                Encoding.Exchange.encode
        let playStrats =
            getFromAdvantage
                model.PlayoutModel
                playInfoSets
                Encoding.Playout.encode

            // weave strategies back together in the right order
        let strategies =
            (0, 0, 0)
                |> Array.unfold (fun (stratIdx, passIdx, playIdx) ->
                    if passIdx < passIdxs.Length
                        && passIdxs[passIdx] = stratIdx then
                        let strat = passStrats[passIdx]
                        let state = stratIdx + 1, passIdx + 1, playIdx
                        Some (strat, state)
                    elif playIdx < playIdxs.Length
                        && playIdxs[playIdx] = stratIdx then
                        let strat = playStrats[playIdx]
                        let state = stratIdx + 1, passIdx, playIdx + 1
                        Some (strat, state)
                    else
                        assert(stratIdx = infoSets.Length)
                        None)
        assert(strategies.Length = infoSets.Length)
        strategies

    /// Creates a Hearts player using the given model.
    let createPlayer model =

        let rng = System.Random()   // each player has its own RNG

        let act infoSet =
            let strategy =
                getFromModel model [|infoSet|]
                    |> Array.exactlyOne
            Vector.sample rng strategy
                |> Array.get infoSet.LegalActions

        { Act = act }
