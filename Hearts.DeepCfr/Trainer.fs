﻿namespace Hearts.DeepCfr

open MathNet.Numerics.LinearAlgebra
open TorchSharp
open Hearts

module Trainer =

    /// Assume two-player, zero-sum game.
    let private numPlayers = 2

    let private tryGetPayoff deal =
        Game.tryUpdateScore deal Score.zero
            |> Option.map (fun score ->
                let otherAvg =
                    (score.ScoreMap
                        |> Map.toSeq
                        |> Seq.where (fun (seat, _) -> seat <> Seat.South)
                        |> Seq.sumBy snd
                        |> float)
                        / float (Seat.numSeats - 1)
                otherAvg - float score[Seat.South])

    /// Computes strategy for the given info set using the
    /// given advantage model.
    let getStrategy infoSetKey model =
        use _ = torch.no_grad()   // use model.eval() instead?
        (AdvantageModel.getAdvantage infoSetKey model)
            .data<float32>()
            |> DenseVector.ofSeq
            |> InformationSet.getStrategy

    /// Negates opponent's utilties (assuming a zero-zum game).
    let private getActiveUtilities utilities =
        utilities
            |> Seq.map (~-)
            |> DenseVector.ofSeq

    /// Evaluates the utility of the given deal.
    let private traverse iter deal updatingPlayer (models : _[]) =

        /// Appends an item to the end of an array.
        let append items item =
            [| yield! items; yield item |]

        /// Top-level loop.
        let rec loop deal =
            match tryGetPayoff deal with
                | Some payoff ->
                    float32 payoff, Array.empty   // game is over
                | None ->
                    loopNonTerminal deal

        /// Recurses for non-terminal game state.
        and loopNonTerminal (deal : OpenDeal) =

                // get info set for current state from this player's point of view
            let activePlayer =
                if OpenDeal.currentPlayer deal = Seat.South then 0
                else 1
            let hand = OpenDeal.currentHand deal
            let infoSetKey =
                {
                    Hand = hand
                    Deal = deal.ClosedDeal
                }

                // get active player's current strategy for this info set
            let strategy =
                getStrategy infoSetKey models[activePlayer]

                // get utility of this info set
            if activePlayer = updatingPlayer then

                    // get utility of each action
                let actionUtilities, samples =
                    let utilities, sampleArrays =
                        deal.ClosedDeal
                            |> ClosedDeal.legalPlays hand
                            |> Seq.toArray
                            |> Array.map (fun play ->
                                deal
                                    |> OpenDeal.addPlay play
                                    |> loop)
                            |> Array.unzip
                    getActiveUtilities utilities,
                    Array.concat sampleArrays

                    // utility of this info set is action utilities weighted by action probabilities
                let utility = actionUtilities * strategy
                let sample =
                    AdvantageSample.create
                        infoSetKey
                        (actionUtilities - utility)
                        iter |> Choice1Of2
                utility, append samples sample

            else
                    // sample a single action according to the strategy
                let utility, samples =
                    let plays =
                        deal.ClosedDeal
                            |> ClosedDeal.legalPlays hand
                            |> Seq.toArray
                    let play =
                        strategy
                            |> Vector.sample settings.Random
                            |> Array.get plays
                    deal
                       |> OpenDeal.addPlay play
                       |> loop
                let sample =
                    StrategySample.create
                        infoSetKey
                        strategy
                        iter |> Choice2Of2
                -utility, append samples sample

        loop deal |> snd

    /// Advantage state managed for each player.
    type private AdvantageState =
        {
            /// Player's model.
            Model : AdvantageModel

            /// Player's reservoir.
            Reservoir : Reservoir<AdvantageSample>
        }

    module private AdvantageState =

        /// Creates an advantage model.
        let private createModel () =
            AdvantageModel.create
                settings.HiddenSize
                settings.LearningRate

        /// Creates an advantage state.
        let create () =
            {
                Model = createModel ()
                Reservoir =
                    Reservoir.create
                        settings.Random
                        settings.NumAdvantageSamples
            }

        /// Resets the model of the given state.
        let resetModel state =
            {
                state with
                    Model = createModel ()
            }

    /// Generates training data for the given player.
    let private generateSamples iter updatingPlayer stateMap =
        Choice.unzip [|
            for _ = 1 to settings.NumTraversals do
                let deal =
                    let iDeal =
                        settings.Random.Next(
                            KuhnPoker.allDeals.Length)
                    KuhnPoker.allDeals[iDeal]
                let models =
                    stateMap
                        |> Map.values
                        |> Seq.map (fun state -> state.Model)
                        |> Seq.toArray
                yield! traverse
                    iter deal updatingPlayer models
        |]

    /// Adds the given samples to the given reservoir and then
    /// uses the reservoir to train the given advantage model.
    let private trainAdvantageModel state newSamples =
        let resv =
            Reservoir.addMany newSamples state.Reservoir
        let losses =
            AdvantageModel.train
                settings.NumAdvantageTrainSteps
                resv.Items
                state.Model
        resv, losses

    /// Trains a single iteration.
    let private trainIteration iter stateMap =

            // train each player's model
        let stratSampleSeqs, resvMap =
            (stateMap, seq { 0 .. KuhnPoker.numPlayers - 1 })
                ||> Seq.mapFold (fun stateMap updatingPlayer ->

                        // generate training data for this player
                    let advSamples, stratSamples =
                        generateSamples iter updatingPlayer stateMap

                        // train this player's model
                    let state =
                        stateMap[updatingPlayer]
                            |> AdvantageState.resetModel
                    let resv, losses =
                        trainAdvantageModel state advSamples
                    let stateMap =
                        let state = { state with Reservoir = resv }
                        Map.add updatingPlayer state stateMap

                        // log inputs and losses
                    settings.Writer.add_scalar(
                        $"advantage reservoir/player{updatingPlayer}",
                        float32 resv.Items.Count,
                        iter)
                    for step = 0 to losses.Length - 1 do
                        settings.Writer.add_scalar(
                            $"advantage loss/iter%04d{iter}/player{updatingPlayer}",
                            losses[step], step)

                    stratSamples, stateMap)

            // log betting behavior
        for infoSetKey in [ "J"; "K"; "Jc"; "Qb"; "Qcb" ] do
            let betProb =
                let model =
                    let player = (infoSetKey.Length - 1) % 2
                    stateMap[player].Model
                (getStrategy infoSetKey model)[0]
            settings.Writer.add_scalar(
                $"advantage bet probability/{infoSetKey}",
                betProb,
                iter)

        resvMap, Seq.concat stratSampleSeqs

    /// Trains a strategy model using the given samples.
    let private trainStrategyModel (resv : Reservoir<_>) =
        let model =
            StrategyModel.create
                settings.HiddenSize
                settings.LearningRate
        let losses =
            StrategyModel.train
                settings.NumStrategyTrainSteps
                resv.Items
                model
        for step = 0 to losses.Length - 1 do
            settings.Writer.add_scalar(
                "strategy loss", losses[step], step)
        model

    /// Trains for the given number of iterations.
    let train () =

            // create advantage state
        let advStateMap =
            Map [|
                for player = 0 to numPlayers - 1 do
                    player, AdvantageState.create ()
            |]

            // run the iterations
        let _, stratResv =
            let stratResv =
                Reservoir.create
                    settings.Random
                    settings.NumStrategySamples
            let iterNums = seq { 0 .. settings.NumIterations - 1 }
            ((advStateMap, stratResv), iterNums)
                ||> Seq.fold (fun (advStateMap, stratResv) iter ->
                    let advResvMap, stratSamples =
                        trainIteration iter advStateMap
                    let stratResv =
                        Reservoir.addMany stratSamples stratResv
                    settings.Writer.add_scalar(
                        $"strategy reservoir",
                        float32 stratResv.Items.Count,
                        iter)
                    advResvMap, stratResv)

            // train the final strategy model
        trainStrategyModel stratResv
