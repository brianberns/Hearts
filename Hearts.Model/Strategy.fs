namespace Hearts.Model

open System

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

    /// Computes strategies for the given info sets using the
    /// given advantage model.
    let getFromAdvantage model infoSets =

        if Array.length infoSets > 0 then

                // run model on GPU
            use advantages =
                AdvantageModel.getAdvantages infoSets model
            assert(advantages.shape[0] = infoSets.Length)

                // access data on CPU
            let nCols = int advantages.shape[1]
            assert(nCols = Model.outputSize)
            let data =
                use advantages =   // avoid memory leak (https://github.com/dotnet/TorchSharp/issues/1517)
                    advantages.``to``(
                        TorchSharp.torch.CPU, disposeAfter=true)
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

    /// Computes strategy for the given info set using the
    /// given advantage model.
    let private getStrategy model infoSet =
        getFromAdvantage model [|infoSet|]
            |> Array.exactlyOne

    /// Creates a thread-safe, non-deterministic Hearts player
    /// using the given model.
    let createRandomPlayer model =

        let rng = Random.Shared

        let act infoSet =
            getStrategy model infoSet
                |> Vector.sample rng
                |> Array.get infoSet.LegalActions

        { Act = act }

    /// Groups the given info set's legal passes into classes of
    /// equivalent actions. When passing, every card is its own
    /// class.
    let private passClasses infoSet =
        infoSet.LegalActions
            |> Array.indexed
            |> Array.sortBy (fun (_, card) ->
                card.Suit, card.Rank)
            |> Array.map List.singleton

    /// Groups the given info set's legal plays into classes of
    /// equivalent cards: cards of the same suit such that every
    /// card ranked between them, inclusive, has the same point
    /// value, and no card between them might be held by an
    /// opponent. Answers each card with its index among the legal
    /// actions. Classes are ordered by suit, then rank, and each
    /// class's cards by rank.
    let private playClasses (infoSet : InformationSet) =

        /// Is the given card unaccounted for?
        let isOutstanding card =
            infoSet.Deal.UnplayedCards.Contains(card)
                && not (infoSet.Hand.Contains(card))

        /// Are all cards in the given range equivalent?
        let isRun (low : Card) (high : Card) =
            assert(low.Suit = high.Suit)
            assert(low.Rank <= high.Rank)
            assert(not (isOutstanding low))
            assert(not (isOutstanding high))
            let lowValue = Card.pointValue low
            [ int low.Rank .. int high.Rank ]   // inclusive of both cards
                |> Seq.forall (fun r ->
                    let card = Card(enum<Rank> r, low.Suit)
                    Card.pointValue card = lowValue
                        && not (isOutstanding card))

        infoSet.LegalActions
            |> Array.indexed
            |> Array.groupBy (fun (_, card) -> card.Suit)
            |> Array.sortBy fst   // sort groups by suit for determinism
            |> Array.collect (fun (_, group) ->
                group
                    |> Array.sortByDescending (fun (_, card) ->
                        card.Rank)
                    |> Array.fold (fun classes (i, card) ->
                        match classes with
                            | ((_, lowest) :: _ as cls) :: rest
                                when isRun card lowest ->
                                ((i, card) :: cls) :: rest   // join the current class
                            | _ -> [ i, card ] :: classes)   // start a new class
                        []
                    |> List.toArray)

    /// Creates a deterministic Hearts player using the given
    /// model. It chooses the lowest card in the class of
    /// equivalent cards with the highest probability. Ties go
    /// to the first class, by suit and then rank.
    let createPlayer model =

        let act infoSet =
            let strategy = getStrategy model infoSet
            let equivClasses =
                match infoSet.LegalActionType with
                    | ActionType.Pass -> passClasses infoSet
                    | ActionType.Play -> playClasses infoSet
            equivClasses
                |> Array.maxBy (
                    List.sumBy (fun (i, _) -> strategy[i]))   // choose best class
                |> List.head                                  // choose lowest card in best class
                |> snd

        { Act = act }
