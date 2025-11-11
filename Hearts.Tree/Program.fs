namespace Hearts.Tree

open System
open Hearts

module Lowest =

    let act infoSet =
        Array.head infoSet.LegalActions

    let player = { Act = act }

module Highest =

    let act infoSet =
        Array.last infoSet.LegalActions

    let player = { Act = act }

module Random =

    let rng = Random(0)

    let act infoSet =
        let idx = rng.Next(infoSet.LegalActions.Length)
        infoSet.LegalActions[idx]

    let player = { Act = act }

module Program =

    let score, payoff =
        Tournament.run
            (Random(0))
            100000
            Highest.player
            Random.player

    for (KeyValue(seat, points)) in score.ScoreMap do
        printfn $"%-6s{string seat}: {points}"
    printfn $"Payoff: %0.5f{payoff}"
