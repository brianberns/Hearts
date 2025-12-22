namespace Hearts.Cfr

open System
open System.Diagnostics

open Microsoft.Data.Sqlite
open FastCfr
open PlayingCards
open Hearts

/// Model Hearts as a zero-sum game.
module ZeroSum =

    /// Gets the payoff for the given raw score from each
    /// player's point of view.
    let getPayoff score =
        let points = score.Points
        assert(points.Length = Seat.numSeats)
        let sum = Seq.sum points
        [|
            for pt in points do
                let otherAvg =
                    float32 (sum - pt)
                        / float32 (Seat.numSeats - 1)
                otherAvg - float32 pt
        |]

module Program =

    let private createTerminalGameState score =
        ZeroSum.getPayoff score
            |> TerminalGameState.create
            |> Terminal

    let rec private createNonTerminalGameState deal =
        let infoSet = OpenDeal.currentInfoSet deal Score.zero
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
        let game = Game.create deal
        match Game.tryUpdateScore game with
            | Some game ->
                createTerminalGameState game.Score
            | None ->
                createNonTerminalGameState deal

    /// Generates an infinite sequence of deals.
    let generate (rng : Random) =
        Seq.initInfinite (fun iDeal ->
            let dir =
                enum<ExchangeDirection>
                    (iDeal % ExchangeDirection.numDirections)
            let deck = Deck.shuffle rng
            let dealer = enum<Seat> (iDeal % Seat.numSeats)
            OpenDeal.fromDeck dealer dir deck)   // ignore effect of game score

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
            conn.CreateCommand(
                CommandText =
                    """
                    insert into Strategy (Key, Action)
                    values ($Key, $Action)
                    """,
                Transaction = conn.BeginTransaction())
        let keyParam = cmd.Parameters.Add("$Key", SqliteType.Text)
        let actionParam = cmd.Parameters.Add("$Action", SqliteType.Integer)
        cmd.Prepare()

        for (key : string, infoSet) in Map.toSeq infoSetMap do
            keyParam.Value <- key
            actionParam.Value <-
                infoSet.StrategySum
                    |> Seq.indexed
                    |> Seq.maxBy snd
                    |> fst
            let nRows = cmd.ExecuteNonQuery()
            assert(nRows = 1)
        cmd.Transaction.Commit()

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

        printfn "Iteration, # Info Sets, Duration (ms), Saved"
        let stopwatch = Stopwatch.StartNew()
        for (iter, state) in Seq.indexed tuples do
            printf $"{iter}, {state.InfoSetMap.Count}, {stopwatch.ElapsedMilliseconds}"
            if iter % 10 = 0 then
                saveStrategy state.InfoSetMap
                printfn ", saved"
            else
                printfn ""
            stopwatch.Restart()

    Console.OutputEncoding <- System.Text.Encoding.UTF8
    printfn $"Server garbage collection: {Runtime.GCSettings.IsServerGC}"
    run ()
