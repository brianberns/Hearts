namespace Hearts.DeepCfr

open System
open System.Data.SQLite

open MathNet.Numerics.Distributions

open Hearts
open Hearts.FastCfr

module Champion =

    /// Database connection.
    let private conn =
        let connStr = "DataSource=./Hearts.db;Version=3;"
        new SQLiteConnection(connStr)
    do conn.Open()

    /// Finds the strategy for the given key, if any.
    let private tryGetStrategy (key : byte[]) =
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

    /// Plays a card from the given hand in the given deal.
    let private play hand deal =
        let index =
            GameState.getInfoSetKey hand deal
                |> tryGetStrategy
                |> Option.map (fun strategy ->
                    Categorical.Sample(settings.Random, strategy))
                |> Option.defaultValue 0
        deal
            |> ClosedDeal.legalPlays hand
            |> Seq.item index

    /// Champion player.
    let player =
        { Play = play }
