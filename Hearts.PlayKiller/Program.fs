namespace Hearts.PlayKiller

open Hearts
open Hearts.Model

module Program =

    let model =

        let exchangeModel =
            new ExchangeModel(
                hiddenSize = Encoding.Exchange.encodedLength * 6,
                numHiddenLayers = 1,
                device = TorchSharp.torch.CPU)
        exchangeModel.load("ExchangeModel.pt") |> ignore

        let playoutModel =
            new PlayoutModel(
                hiddenSize = Encoding.Playout.encodedLength * 6,
                numHiddenLayers = 1,
                device = TorchSharp.torch.CPU)
        playoutModel.load("PlayoutModel.pt") |> ignore

        {
            ExchangeModel = exchangeModel
            PlayoutModel = playoutModel
        }

    let player = Strategy.createPlayer model

    let payoffMap = Killer.run player
    for (KeyValue(seat, payoff)) in payoffMap do
        printfn "%A: %g" seat payoff
