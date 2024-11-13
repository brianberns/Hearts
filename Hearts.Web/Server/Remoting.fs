namespace Hearts.Web

open System
open System.Data.SQLite
open System.IO

open Hearts.FastCfr

open MathNet.Numerics.Distributions

open Fable.Remoting.Server
open Fable.Remoting.Suave

module Remoting =

    /// Hearts API.
    let private heartsApi dir =

        /// Database connection.
        let path = Path.Combine(dir, "Hearts.db")
        let connStr = $"DataSource={path};Version=3;"
        let conn = new SQLiteConnection(connStr)
        conn.Open()

        /// Finds the strategy for the given key, if any.
        let tryGetStrategy (key : byte[]) =
            use cmd =   // each invocation has its own command to support multithreading
                new SQLiteCommand(
                    "select Probabilities \
                    from Strategy \
                    where Key = @Key",
                    conn)
            cmd.Parameters.AddWithValue("Key", key)
                |> ignore
            let value = cmd.ExecuteScalar()
            if isNull value then None
            else
                value :?> byte[]
                    |> Seq.chunkBySize 2   // number of bytes in a Half
                    |> Seq.map (BitConverter.ToHalf >> float)
                    |> Seq.toArray
                    |> Some

        let rng = Random(0)
        {
            GetPlayIndex =
                fun key ->
                    async {
                        return
                            match tryGetStrategy key with
                                | Some strategy ->
                                    Some (Categorical.Sample(rng, strategy))
                                | None ->
                                    printfn $"No strategy for %A{key}"
                                    None
                    }
            GetStrategy =
                fun key -> async { return tryGetStrategy key }
        }

    /// Build API.
    let webPart dir =
        Remoting.createApi()
            |> Remoting.fromValue (heartsApi dir)
            |> Remoting.buildWebPart
