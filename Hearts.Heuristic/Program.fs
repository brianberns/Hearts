namespace Hearts.Heuristic

open Hearts

module Program =
    let numGames = 2000
    let claudeWinRate =
        Tournament.run 0 true numGames Gemini.player Claude.player
    printfn $"Gemini win rate: {-claudeWinRate}"
