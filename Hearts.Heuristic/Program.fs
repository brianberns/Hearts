namespace Hearts.Heuristic

open Hearts

module Program =
    let numGames = 2000
    let claudeWinRate =
        Tournament.run 0 numGames Gemini.player Claude.player
    printfn $"Claude win rate: {-claudeWinRate}"
