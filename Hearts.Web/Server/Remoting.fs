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
    let getStrategy infoSet model =
        let legalPlays =
            InformationSet.legalPlays infoSet
                |> Seq.toArray
        Strategy.getFromAdvantage infoSet model legalPlays

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
                        let strategy = Model.getStrategy infoSet model
                        return Vector.sample rng strategy
                    }
            GetStrategy =
                fun infoSet ->
                    async {
                        let strategy = Model.getStrategy infoSet model
                        return strategy.ToArray()
                            |> Array.map float
                    }
        }

    /// Build API.
    let webPart dir =
        Remoting.createApi()
            |> Remoting.fromValue (heartsApi dir)
            |> Remoting.buildWebPart
