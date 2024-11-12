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
                    Probabilities blob, \
                    primary key (Key))",
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

    /// Converts an array to a blob.
    let toBlob (probs : float[]) =
        [|
            for prob in probs do
                yield! BitConverter.GetBytes(prob)
        |]

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
                "insert into Strategy (Key, Probabilities) \
                values (@Key, @Probabilities)",
                conn)
        let keyParam = strategyCmd.Parameters.Add("Key", DbType.Binary)
        let probsParam = strategyCmd.Parameters.Add("Probabilities", DbType.Binary)

            // insert strategy's rows
        let strategy = Strategy.load "Hearts.strategy"
        printfn $"{strategy.Count} rows to load"
        for (iStrategy, (key, probs)) in strategy |> Map.toSeq |> Seq.indexed do

            let strategyNum = iStrategy + 1
            if strategyNum % 100000 = 0 then
                printfn $"{strategyNum}"

            keyParam.Value <- key
            probsParam.Value <- toBlob probs

            let nRows = strategyCmd.ExecuteNonQuery()
            assert(nRows = 1)

    do
        use conn = connect "Hearts.db"
        load conn
