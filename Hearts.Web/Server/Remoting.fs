namespace Hearts.Web

open System
open System.IO

open Hearts.FastCfr

open MathNet.Numerics.Distributions

open Fable.Remoting.Server
open Fable.Remoting.Suave

module Remoting =

    /// Hearts API.
    let private heartsApi dir =

        let strategyMap =
            let path = Path.Combine(dir, "Hearts.strategy")
            Strategy.load path

        let lookup key =
            Map.tryFind key strategyMap

        let rng = Random(0)

        {
            GetPlayIndex =
                fun key ->
                    async {
                        return
                            match lookup key with
                                | Some strategy ->
                                    Some (Categorical.Sample(rng, strategy))
                                | None ->
                                    printfn $"No strategy for {key}"
                                    None
                    }
            GetStrategy =
                fun key -> async { return lookup key }
        }

    /// Build API.
    let webPart dir =
        Remoting.createApi()
            |> Remoting.fromValue (heartsApi dir)
            |> Remoting.buildWebPart
