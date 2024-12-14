namespace Hearts.DeepCfr

open MathNet.Numerics.LinearAlgebra
open TorchSharp

open PlayingCards
open Hearts

module ZeroSum =

    /// Assume two-player, zero-sum game.
    let numPlayers = 2

    /// Computes the payoff for the given deal, if it is
    /// complete.
    let tryGetPayoff deal =
        Game.tryUpdateScore deal Score.zero
            |> Option.map (fun score ->
                let southPayoff =
                    let otherAvg =
                        (score.ScoreMap
                            |> Map.toSeq
                            |> Seq.where (fun (seat, _) ->
                                seat <> Seat.South)
                            |> Seq.sumBy snd
                            |> float32)
                            / float32 (Seat.numSeats - 1)
                    otherAvg - float32 score[Seat.South]
                [| southPayoff; -southPayoff |])

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
        assert(wide.Count = Card.allCards.Length)
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

    /// Computes strategy for the given info set using the
    /// given advantage model.
    let get hand deal model legalPlays =
        use _ = torch.no_grad()   // use model.eval() instead?
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
    let traverse iter deal updatingPlayer (models : _[]) =

        /// Top-level loop.
        let rec loop deal =
            match ZeroSum.tryGetPayoff deal with
                | Some payoff ->
                    payoff, Array.empty   // game is over
                | None ->
                    loopNonTerminal deal

        /// Recurses for non-terminal game state.
        and loopNonTerminal deal =

                // get info set for current state from this player's point of view
            let activePlayer =
                if OpenDeal.currentPlayer deal = Seat.South then 0
                else 1
            let hand = OpenDeal.currentHand deal

                // get active player's current strategy for this info set
            let legalPlays =
                deal.ClosedDeal
                    |> ClosedDeal.legalPlays hand
                    |> Seq.toArray
            if legalPlays.Length = 1 then
                addLoop deal legalPlays[0]   // forced play
            else
                    // get utility of this info set
                let strategy =
                    Strategy.get
                        hand
                        deal.ClosedDeal
                        models[activePlayer]
                        legalPlays
                let getUtility =
                    if activePlayer = updatingPlayer then getFullUtility
                    else getOneUtility
                getUtility hand deal activePlayer legalPlays strategy

        /// Adds the given play to the given deal and loops.
        and addLoop deal play =
            deal
                |> OpenDeal.addPlay play
                |> loop

        /// Gets the full utility of the given info set.
        and getFullUtility hand deal activePlayer legalPlays strategy =

                // get utility of each action
            let actionUtilities, samples =
                let utilityArrays, sampleArrays =
                    legalPlays
                        |> Array.map (addLoop deal)
                        |> Array.unzip
                utilityArrays
                    |> Seq.map (fun utilities ->
                        utilities[activePlayer])
                    |> DenseVector.ofSeq,
                Array.concat sampleArrays

                // utility of this info set is action utilities weighted by action probabilities
            let utility = actionUtilities * strategy
            let samples =
                let wideRegrets =
                    (actionUtilities - utility)
                        |> Strategy.toWide legalPlays
                AdvantageSample.create
                    hand deal.ClosedDeal wideRegrets iter
                    |> Choice1Of2
                    |> append samples
            let utilities =
                Array.init ZeroSum.numPlayers (fun i ->
                    if i = activePlayer then utility
                    else -utility)
            utilities, samples

        /// Gets the utility of the given info set by sampling
        /// a single action.
        and getOneUtility hand deal _activePlayer legalPlays strategy =

                // sample a single action according to the strategy
            let utilities, samples =
                strategy
                    |> Vector.sample settings.Random
                    |> Array.get legalPlays
                    |> addLoop deal
            let samples =
                let wideStrategy =
                    Strategy.toWide legalPlays strategy
                StrategySample.create
                    hand deal.ClosedDeal wideStrategy iter
                    |> Choice2Of2
                    |> append samples
            utilities, samples

        loop deal |> snd
