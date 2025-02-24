namespace Hearts.Web

open System
open System.IO

open Fable.Remoting.Server
open Fable.Remoting.Suave

open Hearts
open Hearts.Model

module Model =

    /// Performance tweak.
    let private _noGrade = TorchSharp.torch.no_grad()

    /// Connects to Hearts model.
    let connect dir =
        let model =
            new AdvantageModel(TorchSharp.torch.CPU)
        let path = Path.Combine(dir, "AdvantageModel.pt")
        model.load(path) |> ignore
        model

    /// Finds the strategy for the given info set.
    let getStrategy model hand deal =
        let legalPlays =
            deal
                |> ClosedDeal.legalPlays hand
                |> Seq.toArray
        Strategy.getFromAdvantage
            model hand deal legalPlays

module Remoting =

    /// Hearts API.
    let private heartsApi dir =
        let rng = Random(0)
        let model = Model.connect dir
        model.eval()
        {
            GetPlayIndex =
                fun hand deal ->
                    async {
                        let strategy = Model.getStrategy model hand deal
                        return Vector.sample rng strategy
                    }
            GetStrategy =
                fun hand deal ->
                    async {
                        let strategy = Model.getStrategy model hand deal
                        return strategy.ToArray()
                            |> Array.map float
                    }
        }

    /// Build API.
    let webPart dir =
        Remoting.createApi()
            |> Remoting.fromValue (heartsApi dir)
            |> Remoting.buildWebPart
