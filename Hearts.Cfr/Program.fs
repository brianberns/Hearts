namespace Hearts.Cfr

open System
open System.Diagnostics

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
            printfn $"{iChunk}, {state.InfoSetMap.Count}, {stopwatch.ElapsedMilliseconds}"
            stopwatch.Restart()

    Console.OutputEncoding <- System.Text.Encoding.UTF8
    printfn $"Server garbage collection: {Runtime.GCSettings.IsServerGC}"
    run ()
