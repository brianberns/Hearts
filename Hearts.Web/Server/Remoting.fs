namespace Hearts.Web

open System.IO

open Fable.Remoting.Server
open Fable.Remoting.Suave

open Hearts
open Hearts.Learn

module Model =

    /// Connects to Hearts model.
    let connect dir =
        let model = new AdvantageModel()
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
        let model = Model.connect dir
        {
            GetPlayIndex =
                fun hand deal ->
                    async {
                        let strategy = Model.getStrategy model hand deal
                        return Vector.sample settings.Random strategy
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
