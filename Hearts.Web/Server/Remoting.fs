namespace Hearts.Web

open System
open System.IO

open Fable.Remoting.Server
open Fable.Remoting.Suave

open Hearts.Model

module Model =

    /// Server is inference-only, so disable all gradient
    /// calculations.
    let private _noGrade = TorchSharp.torch.no_grad()

    /// Connects to Hearts model.
    let connect dir =

        let exchangeModel =
            new ExchangeModel(
                hiddenSize = Encoding.Exchange.encodedLength * 6,
                numHiddenLayers = 1,
                device = TorchSharp.torch.CPU)
        let path = Path.Combine(dir, "ExchangeModel.pt")
        exchangeModel.load(path) |> ignore

        let playoutModel =
            new PlayoutModel(
                hiddenSize = Encoding.Playout.encodedLength * 6,
                numHiddenLayers = 1,
                device = TorchSharp.torch.CPU)
        let path = Path.Combine(dir, "PlayoutModel.pt")
        exchangeModel.load(path) |> ignore

        {
            ExchangeModel = exchangeModel
            PlayoutModel = playoutModel
        }

module Remoting =

    /// Hearts API.
    let private heartsApi dir =
        let rng = Random(0)
        let model = Model.connect dir
        model.eval()
        {
            GetActionIndex =
                fun infoSet ->
                    async {
                        let strategy =
                            Strategy.getFromModel
                                model
                                [|infoSet|]
                                |> Array.exactlyOne
                        return Vector.sample rng strategy
                    }
            GetStrategy =
                fun infoSet ->
                    async {
                        let strategy =
                            Strategy.getFromModel
                                model
                                [|infoSet|]
                                |> Array.exactlyOne
                        return strategy.ToArray()
                            |> Array.map float
                    }
        }

    /// Build API.
    let webPart dir =
        Remoting.createApi()
            |> Remoting.fromValue (heartsApi dir)
            |> Remoting.buildWebPart
