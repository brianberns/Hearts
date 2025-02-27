namespace Hearts.Model

open MathNet.Numerics.LinearAlgebra

open PlayingCards

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
    /// a narrow vector (indexed by legal plays).
    let toNarrow (legalPlays : _[]) (wide : Vector<_>) =
        assert(wide.Count = Card.numCards)
        legalPlays
            |> Seq.map (
                Card.toIndex >> Vector.get wide)
            |> DenseVector.ofSeq

    /// Converts a narrow vector (indexed by legal plays) to
    /// a wide vector (indexed by entire deck).
    let toWide (legalPlays : _[]) (narrow : Vector<float32>) =
        assert(narrow.Count = legalPlays.Length)
        Seq.zip legalPlays narrow
            |> Encoding.encodeCardValues
            |> DenseVector.ofArray

    /// Computes strategy for the given info set (hand + deal)
    /// using the given advantage model.
    let getFromAdvantage model hand deal legalPlays =
        use advantage =
            AdvantageModel.getAdvantage hand deal model
        advantage.data<float32>()
            |> DenseVector.ofSeq
            |> toNarrow legalPlays
            |> matchRegrets
