namespace Hearts.Cfr

open System
open System.Diagnostics
open System.Text

open FastCfr

open Hearts
open Hearts.Model

module Encoding =

    /// "Latin Extended-A" block is printable.
    let private charOffset = 0x100

    /// Converts a byte array to a compact, printable Unicode string.
    let private compact bytes =
        bytes
            |> Array.map (fun (b : byte) ->
                char (int b + charOffset))
            |> String

    let toString (encoding : Encoding) =
        assert(encoding.Length = Encoding.encodedLength)
        let bytes =
            let nBytes = (encoding.Length + 7) >>> 3
            Array.zeroCreate<byte> nBytes
        encoding.CopyTo(bytes, 0)
        compact bytes

module Program =

    let private focusSeat = Seat.South
    let private focusPlayerIdx = 0
    let private _otherPlayerIdx = 1

    let private createTerminalGameState score =
        let focusPoints, otherPoints =
            ((0, 0), Map.toSeq score.ScoreMap)
                ||> Seq.fold (fun (focusPoints, otherPoints) (seat, point) ->
                    if seat = focusSeat then
                        focusPoints + point, otherPoints
                    else
                        focusPoints, otherPoints + point)
        let focusPayoff =
            (float32 otherPoints / float32 (Seat.numSeats - 1))
                - float32 focusPoints
        TerminalGameState.create focusPlayerIdx focusPayoff
            |> Terminal

    let rec private createNonTerminalGameState deal =
        let infoSet = OpenDeal.currentInfoSet deal
        NonTerminal {
            ActivePlayerIdx =
                if infoSet.Player = Seat.South then 0
                else 1
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

        Console.OutputEncoding <- Encoding.UTF8
        printfn $"Server garbage collection: {Runtime.GCSettings.IsServerGC}"

        let chunkSize = 80
        let numChunks = 1000
        let numDeals = chunkSize * numChunks
        printfn $"Number of deals: {numDeals}"
        printfn $"Chunk size: {chunkSize}"

        let stopwatch = Stopwatch.StartNew()
        let rng = Random(0)
        let tuples =
            OpenDeal.generate rng numDeals createGameState
                |> Seq.chunkBySize chunkSize
                |> Trainer.trainScan

        for (iChunk, (infoSetMap, nGames, utilities)) in Seq.indexed tuples do
            printfn ""
            printfn $"Chunk: {iChunk}"
            printfn $"Elapsed time: {stopwatch.Elapsed}"
            stopwatch.Restart()

            let utility = utilities / float32 nGames
            printfn $"Utility: {utility}"

            let visitCounts =
                infoSetMap.Values
                    |> Seq.groupBy _.NumVisits
                    |> Seq.map (fun (nVisits, group) ->
                        {|
                            NumVisits = nVisits
                            Count = Seq.length group
                        |})
                    |> Seq.sortBy _.NumVisits
            printfn "# visits, Count"
            for visitCount in visitCounts do
                printfn $"{visitCount.NumVisits}, {visitCount.Count}"

    run ()
