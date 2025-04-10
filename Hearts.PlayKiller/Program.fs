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

    let payoffMap = Killer.run player
    for (KeyValue(seat, payoff)) in payoffMap do
        printfn "%A: %g" seat payoff
