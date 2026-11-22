namespace Hearts.Heuristic

open System
open Hearts

module Program =
    let numGames = 2000
    let claudePayoff =
        Tournament.run 0 true numGames Gemini.player Claude.player
    printfn $"Gemini payoff: {-claudePayoff}"
    printfn $"Claude payoff: {claudePayoff}"
