namespace Hearts.FastCfr.LoadDatabase

open System
open System.Data
open System.Data.SQLite

open Hearts.FastCfr

module Program =

    /// Creates database schema.
    let createSchema conn =
        use cmd =
            new SQLiteCommand(
                "create table Strategy ( \
                    Key blob, \
                    ActionIndex integernot null, \
                    Probability real not null, \
                    primary key (Key, ActionIndex))",
                conn)
        cmd.ExecuteNonQuery() |> ignore

    /// Creates and connects to database.
    let connect dbFileName =

            // always create a new database (WARNING: overrides existing database, if any)
        SQLiteConnection.CreateFile(dbFileName)

            // open connection
        let connStr = sprintf "DataSource=%s;Version=3;" dbFileName
        let conn = new SQLiteConnection(connStr)
        conn.Open()

            // create schema
        createSchema conn

        conn

    /// Converts the given option to a database-safe value.
    let safeValue valueOpt =
        valueOpt
            |> Option.map (fun value -> value :> obj)
            |> Option.defaultValue (DBNull.Value :> _)

    /// Creates and loads database.
    let load conn =

            // enable bulk load
        use pragmaCmd =
            new SQLiteCommand(
                "PRAGMA journal_mode = OFF; \
                PRAGMA synchronous = OFF",
                conn)
        pragmaCmd.ExecuteNonQuery() |> ignore

            // prepare insert command
        use strategyCmd =
            new SQLiteCommand(
                "insert into Strategy (Key, ActionIndex, Probability) \
                values (@Key, @ActionIndex, @Probability)",
                conn)
        let keyParam = strategyCmd.Parameters.Add("Key", DbType.Binary)
        let actionIdxParam = strategyCmd.Parameters.Add("ActionIndex", DbType.Int32)
        let probParam = strategyCmd.Parameters.Add("Probability", DbType.Double)

            // insert strategy's rows
        let strategy = Strategy.load "Hearts.strategy"
        printfn $"{strategy.Count} rows to load"
        for (iStrategy, (key, probs)) in strategy |> Map.toSeq |> Seq.indexed do

            let strategyNum = iStrategy + 1
            if strategyNum % 100000 = 0 then
                printfn $"{strategyNum}"

            keyParam.Value <- key

            for (iProb, prob) in Seq.indexed probs do

                actionIdxParam.Value <- iProb
                probParam.Value <- prob

                let nRows = strategyCmd.ExecuteNonQuery()
                assert(nRows = 1)

    do
        use conn = connect "Hearts.db"
        load conn
