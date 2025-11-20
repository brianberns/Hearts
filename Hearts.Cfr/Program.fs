namespace Hearts.Cfr

open System
open System.Diagnostics

open Microsoft.Data.Sqlite
open FastCfr
open Hearts

module Program =

    let private createTerminalGameState score =
        ZeroSum.getPayoff score
            |> TerminalGameState.create
            |> Terminal

    let rec private createNonTerminalGameState deal =
        let infoSet = OpenDeal.currentInfoSet deal
        NonTerminal {
            ActivePlayerIdx = int infoSet.Player
            InfoSetKey =
                infoSet
                    |> Encoding.encode
                    |> Encoding.toString
            LegalActions = infoSet.LegalActions
            AddAction =
                fun action ->
                    let deal =
                        OpenDeal.addAction
                            infoSet.LegalActionType action deal
                    createGameState deal
        }

    and private createGameState deal =
        match Game.tryUpdateScore deal Score.zero with
            | Some score ->
                createTerminalGameState score
            | None ->
                createNonTerminalGameState deal

    let executeCmd (conn : SqliteConnection) sql =
        use cmd = conn.CreateCommand(CommandText = sql)
        cmd.ExecuteNonQuery() |> ignore

    let saveStrategy infoSetMap =
        use conn = new SqliteConnection("Data Source=Hearts.db")
        conn.Open()

        executeCmd conn "drop table if exists Strategy"
        executeCmd conn
            """
            create table Strategy (
                Key text primary key,
                Action integer not null)
            """
        executeCmd conn
            """
            pragma journal_mode = OFF;
            pragma synchronous = OFF
            """

            // prepare insert command
        use cmd =
            conn.CreateCommand(CommandText =
                """
                insert into Strategy (Key, Action)
                values ($Key, $Action)
                """)
        let keyParam = cmd.Parameters.Add("$Key", SqliteType.Text)
        let actionParam = cmd.Parameters.Add("$Action", SqliteType.Integer)

        for (key : string, infoSet) in Map.toSeq infoSetMap do
            keyParam.Value <- key
            actionParam.Value <-
                infoSet.StrategySum
                    |> Seq.indexed
                    |> Seq.maxBy snd
                    |> fst
            let nRows = cmd.ExecuteNonQuery()
            assert(nRows = 1)

    let run () =

            // settings for this run
        let chunkSize = 100
        printfn $"Chunk size: {chunkSize}"

            // train on chunks of deals lazily
        let tuples =
            let rng = Random(0)
            OpenDeal.generate rng
                |> Seq.map createGameState
                |> Seq.chunkBySize chunkSize
                |> Trainer.trainScan Seat.numSeats

        let stopwatch = Stopwatch.StartNew()
        for (iChunk, state) in Seq.indexed tuples do
            printf $"{iChunk}, {state.InfoSetMap.Count}, {stopwatch.ElapsedMilliseconds}"
            saveStrategy state.InfoSetMap
            printfn ", saved"
            stopwatch.Restart()

    Console.OutputEncoding <- System.Text.Encoding.UTF8
    printfn $"Server garbage collection: {Runtime.GCSettings.IsServerGC}"
    run ()
