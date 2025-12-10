namespace Hearts.TrainKiller

open System
open System.Diagnostics
open System.IO
open System.Text

open Hearts
open Hearts.Learn
open Hearts.Model
open Hearts.ParseKiller

module Program =

    let getExchangeActions exchangeOpt deal =
        exchangeOpt
            |> Option.map (fun exchange ->
                let player = OpenDeal.currentPlayer deal
                seq {
                    for seat in Seat.cycle player do
                        yield! exchange.PassMap[seat]
                })
            |> Option.defaultValue Seq.empty
            |> Seq.map (fun card -> ActionType.Pass, card)

    let getPlayoutActions tricks =
        seq {
            for trick in tricks do
                for _, card in Trick.plays trick do
                    ActionType.Play, card
        }

    let evaluate iter infoSetPairs model =
        let player = Strategy.createPlayer model
        let correctPairs =
            infoSetPairs
                |> Array.where (fun (infoSet, card) ->
                    player.Act infoSet = card)
        let accuracy = float32 correctPairs.Length / float32 infoSetPairs.Length
        if settings.Verbose then
            printfn $"Accuracy: {accuracy}"
        settings.Writer.add_scalar($"accuracy", accuracy, iter)

    let train () =

        let stopwatch = Stopwatch.StartNew()
        let entries =
            Json.loadEntries @"C:\Users\brian\OneDrive\Desktop\KHearts.zero.json"
        printfn $"Loaded {entries.Length} deals in {stopwatch.Elapsed}"

        let infoSetPairs =
            Array.randomShuffle [|
                for entry in entries do
                    let actions =
                        [|
                            yield! getExchangeActions
                                entry.ExchangeOpt entry.InitialDeal
                            yield! getPlayoutActions entry.Tricks
                        |]
                    let deals =
                        (entry.InitialDeal, actions)
                            ||> Array.scan (fun deal (actionType, card) ->
                                OpenDeal.addAction actionType card deal)
                    assert(deals.Length = actions.Length + 1)
                    for deal, (actionType, card) in Seq.zip deals actions do   // ignore final deal state
                        let infoSet = OpenDeal.currentInfoSet deal
                        assert(infoSet.LegalActionType = actionType)
                        assert(infoSet.LegalActions |> Array.contains card)
                        if infoSet.LegalActions.Length > 1 then
                            infoSet, card
            |]
        printfn $"Extracted {infoSetPairs.Length} info sets"

        let nTestInfoSets = 100000
        let testInfoSetPairs = Array.take nTestInfoSets infoSetPairs
        let trainInfoSetPairs = Array.skip nTestInfoSets infoSetPairs

        let model =
            new AdvantageModel(
                settings.HiddenSize,
                settings.NumHiddenLayers,
                settings.DropoutRate,
                settings.Device)

        let samples =
            [|
                for infoSet, card in trainInfoSetPairs do
                    let input = Encoding.encode infoSet
                    let output = Encoding.encodeCards [card] |> Encoding
                    {
                        Encoding = input
                        Regrets =
                            Encoding.toFloat32 output
                                |> MathNet.Numerics.LinearAlgebra.DenseVector.ofArray
                        Weight = 1.0f
                    }
            |]

        for iter = 1 to 1000 do

            stopwatch.Restart()
            AdvantageModel.train iter samples model
            stopwatch.Stop()
            if settings.Verbose then
                printfn $"Trained model on {samples.Length} samples in {stopwatch.Elapsed} \
                    (%.2f{float stopwatch.ElapsedMilliseconds / float samples.Length} ms/sample)"

            evaluate iter testInfoSetPairs model
            Trainer.evaluate iter model

            Path.Combine(
                settings.ModelDirPath,
                $"KHeartsModel%03d{iter}.pt")
                    |> model.save
                    |> ignore

    Console.OutputEncoding <- Encoding.Unicode
    train ()
