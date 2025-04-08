namespace Hearts.PlayKiller

open Hearts
open Hearts.Model

module Program =

    let model =
        new AdvantageModel(
            hiddenSize = Encoding.encodedLength * 6,
            numHiddenLayers = 1,
            device = TorchSharp.torch.CPU)
    model.load("AdvantageModel.pt") |> ignore

    let player = Strategy.createPlayer model

    let score = Killer.run player
    for (KeyValue(seat, points)) in score.ScoreMap do
        printfn "%A: %d" seat points
