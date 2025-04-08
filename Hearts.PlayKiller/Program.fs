namespace Hearts.PlayKiller

open System
open System.IO

open Hearts
open Hearts.Model

module Program =

    let rng = Random(0)

    let model =
        new AdvantageModel(
            hiddenSize = Encoding.encodedLength * 6,
            numHiddenLayers = 1,
            device = TorchSharp.torch.CPU)
    model.load("AdvantageModel.pt") |> ignore

    let act infoSet =
        let strategy =
            Strategy.getFromAdvantage
                model
                [|infoSet|]
                |> Array.exactlyOne
        let idx = Vector.sample rng strategy
        infoSet.LegalActionType,
        infoSet.LegalActions[idx]

    let player = { Act = act }

    let score = Killer.run player
    for (KeyValue(seat, points)) in score.ScoreMap do
        printfn "%A: %d" seat points
