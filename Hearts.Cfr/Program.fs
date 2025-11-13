namespace Hearts.Cfr

open System
open System.Diagnostics

open FastCfr

open Hearts

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
            (float otherPoints / float (Seat.numSeats - 1))
                - float focusPoints
        TerminalGameState.create focusPlayerIdx focusPayoff
            |> Terminal

    let rec private createNonTerminalGameState deal =
        let infoSet = OpenDeal.currentInfoSet deal
        NonTerminal {
            ActivePlayerIdx =
                if infoSet.Player = Seat.South then 0
                else 1
            InfoSetKey = infoSet
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

        printfn $"Server garbage collection: {Runtime.GCSettings.IsServerGC}"

        let chunkSize = 16
        let numChunks = 4
        let numDeals = chunkSize * numChunks
        printfn $"Number of deals: {numDeals}"
        printfn $"Chunk size: {chunkSize}"

        let rng = Random(0)
        let utility, infoSetMap =
            OpenDeal.generate rng numDeals createGameState
                |> Seq.chunkBySize chunkSize
                |> Seq.mapi (fun iChunk chunk ->
                    printfn $"Chunk {iChunk}"
                    chunk)
                |> Trainer.train
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

    let stopwatch = Stopwatch.StartNew()
    run ()
    printfn $"Elapsed time: {stopwatch.Elapsed}"
