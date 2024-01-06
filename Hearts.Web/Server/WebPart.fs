namespace Hearts.Web

module Remoting =

    let private heartsApi dir =
        let profile =
            let path = System.IO.Path.Combine(dir, "Hearts.strategy")
            Cfrm.StrategyProfile.Load(path)
        {
            GetPlayIndex =
                fun key -> async { return profile.Best(key) }
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
