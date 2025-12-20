namespace Hearts.Web

open System
open System.IO

open Fable.Remoting.Server
open Fable.Remoting.Suave

module AdvantageModel =

    open Hearts.Model

    /// Server is inference-only, so disable all gradient
    /// calculations.
    let private _noGrade = TorchSharp.torch.no_grad()

    /// Connects to Hearts model.
    let private connect dir =
        let model =
            new AdvantageModel(
                hiddenSize = Encoding.encodedLength * 2,
                numHiddenLayers = 9,
                dropoutRate = 0.3,
                device = TorchSharp.torch.CPU)
        let path = Path.Combine(dir, "AdvantageModel.pt")
        model.load(path) |> ignore
        model

    /// Hearts API.
    let heartsApi dir =
        let rng = Random(0)
        let model = connect dir
        model.eval()
        {
            GetActionIndex =
                fun infoSet ->
                    async {
                        let strategy =
                            Strategy.getFromAdvantage
                                model
                                [|infoSet|]
                                |> Array.exactlyOne
                        return Vector.sample rng strategy
                    }
            GetStrategy =
                fun infoSet ->
                    async {
                        let strategy =
                            Strategy.getFromAdvantage
                                model
                                [|infoSet|]
                                |> Array.exactlyOne
                        return strategy.ToArray()
                            |> Array.map float
                    }
        }

module Cfr =

    open Hearts.Cfr

    let heartsApi dir =
        {
            GetActionIndex =
                fun infoSet ->
                    async {
                        return Cfr.getActionIndex dir infoSet
                    }
            GetStrategy =
                fun infoSet ->
                    async {
                        let actionIdxOpt = Cfr.tryGetActionIndex dir infoSet
                        let nActions = infoSet.LegalActions.Length
                        return Array.init nActions (fun i ->
                            if Some i = actionIdxOpt then 1
                            else 0)
                    }
        }

module Remoting =

    /// Build API.
    let webPart dir =
        Remoting.createApi()
            |> Remoting.fromValue (AdvantageModel.heartsApi dir)
            |> Remoting.buildWebPart
