namespace Hearts.Heuristic

open System
open Hearts

module Program =
    let numDeals = 200_000
    let rng = Random(0)
    let claudePayoff =
        Tournament.run rng true numDeals Gemini.player Claude.player
    printfn $"Gemini payoff: {-claudePayoff}"
    printfn $"Claude payoff: {claudePayoff}"
