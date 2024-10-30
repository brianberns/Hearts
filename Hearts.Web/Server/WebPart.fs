namespace Hearts.Web

module Remoting =

    open System
    open System.IO
    open Hearts.FastCfr
    open MathNet.Numerics.Distributions

    let private heartsApi dir =
        let strategyMap =
            let path = Path.Combine(dir, "Hearts.strategy")
            Strategy.load path
        let rng = Random(0)
        {
            GetPlayIndex =
                fun key ->
                    async {
                        return strategyMap
                            |> Map.tryFind key
                            |> Option.map (fun strategy ->
                                Categorical.Sample(rng, strategy.ToArray()))
                    }
        }

    open Fable.Remoting.Server
    open Fable.Remoting.Suave

    /// Build API.
    let webPart dir =
        Remoting.createApi()
            |> Remoting.fromValue (heartsApi dir)
            |> Remoting.buildWebPart

module WebPart =

    open System.IO
    open System.Reflection

    open Suave
    open Suave.Filters
    open Suave.Operators

    /// Web part.
    let app =

        let dir =
            Assembly.GetExecutingAssembly().Location
                |> Path.GetDirectoryName
        let staticPath = Path.Combine(dir, "public")

        choose [
            Remoting.webPart dir
            Filters.path "/" >=> Files.browseFile staticPath "index.html"
            GET >=> Files.browse staticPath
        ]
