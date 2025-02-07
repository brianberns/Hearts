namespace Hearts.Web

open Fable.Remoting.Server
open Fable.Remoting.Suave

open Hearts
open Hearts.Learn

module Model =

    let private model =
        let model = new AdvantageModel()
        model.load("AdvantageModel.pt") |> ignore
        model

    let getStrategy hand deal =
        let legalPlays =
            deal
                |> ClosedDeal.legalPlays hand
                |> Seq.toArray
        Strategy.getFromAdvantage
            model hand deal legalPlays

module Remoting =

    /// Hearts API.
    let private heartsApi dir =
        {
            GetPlay =
                fun hand deal ->
                    async {
                        let strategy = Model.getStrategy hand deal
                        return Vector.sample settings.Random strategy
                    }
            GetStrategy =
                fun hand deal ->
                    async {
                        let strategy = Model.getStrategy hand deal
                        return strategy.ToArray()
                            |> Array.map float
                    }
        }

    /// Build API.
    let webPart dir =
        Remoting.createApi()
            |> Remoting.fromValue (heartsApi dir)
            |> Remoting.buildWebPart
