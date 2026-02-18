namespace Hearts.PlayKiller

open Hearts.Model

module Program =

    let model =
        let model =
            new AdvantageModel(
                hiddenSize = Encoding.encodedLength * 2,
                numHiddenLayers = 4,
                dropoutRate = 0.0,
                device = TorchSharp.torch.CPU)
        model.load("AdvantageModel.pt") |> ignore
        model.eval()
        model

    let player = Strategy.createPlayer model

    let payoffMap = Killer.run player
    for (KeyValue(seat, payoff)) in payoffMap do
        printfn "%A: %g" seat payoff
