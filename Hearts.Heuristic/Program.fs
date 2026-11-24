namespace Hearts.Heuristic

open Hearts

module Program =
    let numGames = 2000
    let killerWinRate =
        Tournament.run 0 numGames Gemini.player Killer.player
    printfn $"Killer win rate: {-killerWinRate}"
