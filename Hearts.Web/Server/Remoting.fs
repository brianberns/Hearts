namespace Hearts.Web

open System
open System.IO

open Microsoft.Data.Sqlite

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
                hiddenSize = Encoding.encodedLength * 6,
                numHiddenLayers = 1,
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
                        let path = Path.Combine(dir, "Hearts.db")
                        use conn = new SqliteConnection($"Data Source={path}")
                        conn.Open()

                        use cmd =
                            conn.CreateCommand(
                                CommandText =
                                    "select Action from Strategy where Key = $Key")
                        let key =
                            infoSet
                                |> Encoding.encode
                                |> Encoding.toString
                        cmd.Parameters.AddWithValue("$Key", key)
                            |> ignore

                        let action = cmd.ExecuteScalar()
                        return
                            if isNull action then 0
                            else action :?> int64 |> int
                    }
            GetStrategy =
                fun infoSet ->
                    async {
                        return Array.empty
                    }
        }

module Remoting =

    /// Build API.
    let webPart dir =
        Remoting.createApi()
            |> Remoting.fromValue (Cfr.heartsApi dir)
            |> Remoting.buildWebPart
