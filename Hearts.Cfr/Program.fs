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
        let chunkSize = 200
        printfn $"Chunk size: {chunkSize}"

            // train on chunks of deals lazily
        let tuples =
            let rng = Random(0)
            OpenDeal.generate rng
                |> Seq.map createGameState
                |> Seq.chunkBySize chunkSize
                |> Trainer.trainScan Seat.numSeats

        let stopwatch = Stopwatch.StartNew()
        for (iChunk, (infoSetMap, nGames, utilities)) in Seq.indexed tuples do
            printfn ""
            printfn $"Chunk: {iChunk}"
            printfn $"Elapsed time: {stopwatch.Elapsed}"
            stopwatch.Restart()

            let utility = utilities / float32 nGames
            printfn $"Utility: {utility}"

            let infoSetMap =
                infoSetMap
                    |> Map.filter (fun key _ ->
                        key[0] = '0')

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

    Console.OutputEncoding <- Encoding.UTF8
    printfn $"Server garbage collection: {Runtime.GCSettings.IsServerGC}"
    run ()
