namespace Hearts.Tree

open System
open Hearts

module Program =

    let act (infoSet : InformationSet) =
        infoSet.LegalActions[0]

    let player = { Act = act }

    let score, payoff =
        Tournament.run
            (Random(0))
            1000
            player
            player

    for (KeyValue(seat, points)) in score.ScoreMap do
        printfn $"%-6s{string seat}: {points}"
    printfn $"Payoff: %0.5f{payoff}"
