namespace Hearts.DeepCfr

open System
open System.Diagnostics

open MathNet.Numerics.LinearAlgebra
open TorchSharp

open PlayingCards
open Hearts

module Trainer =

    /// Assume two-player, zero-sum game.
    let private numPlayers = 2

    /// Computes the payoff for the given deal, if it is
    /// complete.
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
    let private getStrategy infoSetKey model indexes =
        use _ = torch.no_grad()   // use model.eval() instead?
        let wide =
            (AdvantageModel.getAdvantage infoSetKey model)
                .data<float32>()
                |> DenseVector.ofSeq
        indexes
            |> Seq.map (fun idx -> wide[idx])
            |> DenseVector.ofSeq
            |> InformationSet.getStrategy

    /// Negates opponent's utilties (assuming a zero-zum game).
    let private getActiveUtilities utilities =
        utilities
            |> Seq.map (~-)
            |> DenseVector.ofSeq

    /// Converts a narrow vector (indexed by legal plays) to
    /// a wide vector (indexed by entire deck).
    let private toWide (legalPlays : _[]) (narrowValues : Vector<_>) =
        assert(narrowValues.Count = legalPlays.Length)
        Seq.zip legalPlays narrowValues
            |> Encoding.encodeCardValues
            |> DenseVector.ofArray

    /// Appends an item to the end of an array.
    let private append items item =
        [| yield! items; yield item |]

    /// Evaluates the utility of the given deal.
    let private traverse iter deal updatingPlayer (models : _[]) =

        /// Top-level loop.
        let rec loop deal =
            match tryGetPayoff deal with
                | Some payoff ->
                    float32 payoff, Array.empty   // game is over
                | None ->
                    loopNonTerminal deal

        /// Recurses for non-terminal game state.
        and loopNonTerminal deal =

                // get info set for current state from this player's point of view
            let activePlayer =
                if OpenDeal.currentPlayer deal = Seat.South then 0
                else 1
            let hand = OpenDeal.currentHand deal
            let infoSetKey = InfoSetKey.create hand deal.ClosedDeal

                // get active player's current strategy for this info set
            let legalPlays =
                deal.ClosedDeal
                    |> ClosedDeal.legalPlays hand
                    |> Seq.toArray
            let strategy =
                legalPlays
                    |> Seq.map Card.toIndex
                    |> getStrategy infoSetKey models[activePlayer]

                // get utility of this info set
            let getUtility =
                if activePlayer = updatingPlayer then getFullUtility
                else getOneUtility
            getUtility deal infoSetKey legalPlays strategy

        /// Adds the given play to the given deal and loops.
        and addLoop deal play =
            deal
                |> OpenDeal.addPlay play
                |> loop

        /// Gets the full utility of the given info set.
        and getFullUtility deal infoSetKey legalPlays strategy =

                // get utility of each action
            let actionUtilities, samples =
                let utilities, sampleArrays =
                    legalPlays
                        |> Array.map (addLoop deal)
                        |> Array.unzip
                getActiveUtilities utilities,
                Array.concat sampleArrays

                // utility of this info set is action utilities weighted by action probabilities
            let utility = actionUtilities * strategy
            let samples =
                if legalPlays.Length > 1 then
                    let wideRegrets =
                        (actionUtilities - utility)
                            |> toWide legalPlays
                    AdvantageSample.create infoSetKey wideRegrets iter
                            |> Choice1Of2
                            |> append samples
                else samples
            utility, samples

        /// Gets the utility of the given info set by sampling
        /// a single action.
        and getOneUtility deal infoSetKey legalPlays strategy =

                // sample a single action according to the strategy
            let utility, samples =
                strategy
                    |> Vector.sample settings.Random
                    |> Array.get legalPlays
                    |> addLoop deal
            let samples =
                if legalPlays.Length > 1 then
                    let wideStrategy =
                        toWide legalPlays strategy
                    StrategySample.create infoSetKey wideStrategy iter
                        |> Choice2Of2
                        |> append samples
                else samples
            -utility, samples

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

        let nDeals =
            let factor =
                if updatingPlayer = 0 then
                    settings.CutthroatCompensation
                else 1
            factor * settings.NumTraversals

        Choice.unzip [|
            for iDeal = 0 to nDeals do
                let deal =
                    let deck = Deck.shuffle settings.Random
                    let dealer = enum<Seat> (iDeal % Seat.numSeats)
                    OpenDeal.fromDeck
                        dealer
                        ExchangeDirection.Hold
                        deck
                        |> OpenDeal.startPlay
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

        let stopwatch = Stopwatch.StartNew()
        let losses =
            AdvantageModel.train resv.Items state.Model
        if settings.Verbose then
            stopwatch.Stop()
            printfn $"   Trained model on {resv.Items.Count} samples in {stopwatch.Elapsed} \
                (%.2f{float stopwatch.ElapsedMilliseconds / float resv.Items.Count} ms/sample)"

        resv, losses

    /// Trains a player.
    let private trainPlayer iter stateMap updatingPlayer =

        if settings.Verbose then
            printfn $"\nTraining player {updatingPlayer}"

            // generate training data for this player
        let advSamples, stratSamples =
            generateSamples iter updatingPlayer stateMap
        if settings.Verbose then
            printfn $"   {advSamples.Length} advantage samples generated"
            printfn $"   {stratSamples.Length} strategy samples generated"

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
        for epoch = 0 to losses.Length - 1 do
            settings.Writer.add_scalar(
                $"advantage loss/iter%04d{iter}/player{updatingPlayer}",
                losses[epoch], epoch)

        stratSamples, stateMap

    let private createChallenger model =

        let play hand deal =
            let legalPlays =
                deal
                    |> ClosedDeal.legalPlays hand
                    |> Seq.toArray
            let strategy =
                let infoSetKey = InfoSetKey.create hand deal
                legalPlays
                    |> Seq.map Card.toIndex
                    |> getStrategy infoSetKey model
            strategy
                |> Vector.sample settings.Random
                |> Array.get legalPlays

        { Play = play }

    /// Trains a single iteration.
    let private trainIteration iter stateMap =

        if settings.Verbose then
            printfn $"\n*** Iteration {iter} ***"

            // train each player's model
        let stratSampleSeqs, stateMap =
            Seq.mapFold
                (trainPlayer iter)
                stateMap
                (seq { 0 .. numPlayers - 1 })

            // evaluate model
        let score =
            let challenger = createChallenger stateMap[0].Model
            Tournament.run
                (Random(0))   // use same deals each iteration
                Tournament.randomPlayer
                challenger
        printfn "\nTournament score:"
        for (KeyValue(seat, points)) in score.ScoreMap do
            printfn $"   {seat}: {points}"

        stateMap, Seq.concat stratSampleSeqs

    /// Trains a strategy model using the given samples.
    let private trainStrategyModel (resv : Reservoir<_>) =

        if settings.Verbose then
            printfn $"\n*** Training strategy model ***"

        let model =
            StrategyModel.create
                settings.HiddenSize
                settings.LearningRate
        let losses =
            StrategyModel.train resv.Items model
        if settings.Verbose then
            printfn $"Trained model on {resv.Items.Count} samples"

        for epoch = 0 to losses.Length - 1 do
            settings.Writer.add_scalar(
                "strategy loss", losses[epoch], epoch)

        model

    /// Trains for the given number of iterations.
    let train () =

        if settings.Verbose then
            printfn $"{settings}"

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
                    let advStateMap, stratSamples =
                        trainIteration iter advStateMap
                    let stratResv =
                        Reservoir.addMany stratSamples stratResv
                    settings.Writer.add_scalar(
                        $"strategy reservoir",
                        float32 stratResv.Items.Count,
                        iter)
                    advStateMap, stratResv)

            // train the final strategy model
        trainStrategyModel stratResv

    let private createTrainingData numDeals =

        let conn = Hearts.Web.Database.connect "."

        let rec loop deal =
            seq {
                let hand =
                    let seat = OpenDeal.currentPlayer deal
                    deal.UnplayedCardMap[seat]
                let legalPlays =
                    ClosedDeal.legalPlays hand deal.ClosedDeal
                        |> Seq.toArray
                let adjustedDeal =
                    Hearts.FastCfr.ClosedDeal.adjustDeal
                        Seat.South
                        deal.ClosedDeal
                let strategyOpt =
                    if legalPlays.Length = 1 then None
                    else
                        adjustedDeal
                            |> Hearts.FastCfr.GameState.getInfoSetKey hand
                            |> Hearts.Web.Database.tryGetStrategy conn
                match strategyOpt with
                    | Some strategy ->
                        let regrets =
                            strategy
                                |> Array.map float32
                                |> DenseVector.ofArray
                                |> toWide legalPlays
                        let infoSetKey =
                            InfoSetKey.create hand adjustedDeal
                        yield AdvantageSample.create infoSetKey regrets 0
                    | None -> ()

                let deal =
                    let card =
                        let index =
                            match strategyOpt with
                                | Some strategy ->
                                    MathNet.Numerics.Distributions.Categorical.Sample(
                                        settings.Random,
                                        strategy)
                                | None -> 0
                        legalPlays[index]
                    OpenDeal.addPlay card deal
                match Game.tryUpdateScore deal Score.zero with
                    | Some _ -> ()
                    | None -> yield! loop deal
            }

        seq {
            for _ = 1 to numDeals do
                let deal =
                    let deck = Deck.shuffle settings.Random
                    OpenDeal.fromDeck
                        Seat.South
                        ExchangeDirection.Hold
                        deck
                        |> OpenDeal.startPlay
                yield! loop deal
        }

    let trainDirect numDeals =

        printfn $"{settings}"
        printfn $"numDeals: {numDeals}"
        let samples =
            createTrainingData numDeals
                |> Seq.toArray
        printfn $"Number of samples: {samples.Length}"

        let model =
            AdvantageModel.create
                settings.HiddenSize
                settings.LearningRate
        let losses = AdvantageModel.train samples model
        printfn $"Final loss {Array.last losses}"

        let pairs =
            [
                "Random", Tournament.randomPlayer
                "Database", Database.player
            ]
        for name, champion in pairs do
            let score =
                let challenger = createChallenger model
                Tournament.run
                    settings.Random
                    champion
                    challenger
            printfn $"\nTournament score vs. {name}:"
            for (KeyValue(seat, points)) in score.ScoreMap do
                printfn $"   {seat}: {points}"
