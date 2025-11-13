namespace Hearts.Cfr

open System
open FastCfr
open Hearts

module Program =

    let rec createGameState deal =
        match ZeroSum.tryGetPayoff deal with
            | Some payoff ->
                TerminalGameState.create
                    0
                    (float payoff[int Seat.South])
                    |> Terminal
            | None ->
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

    printfn $"Server garbage collection: {Runtime.GCSettings.IsServerGC}\n"

    let numDeals = 1
    let chunkSize = 1
    let rng = Random(0)
    let utility, infoSetMap =
        OpenDeal.generate rng numDeals createGameState
            |> Seq.chunkBySize 100
            |> Trainer.train
    printfn $"{utility}"
