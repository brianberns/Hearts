namespace Hearts.Heuristic

open System.Diagnostics
open Hearts

module Program =

    let champion = Killer.player
    let challenger = Random.player
    let numGames = 5000

    let stopwatch = Stopwatch.StartNew()
    let winRate = Tournament.run 0 numGames champion challenger
    printfn $"Challenger win rate: {winRate}"
    printfn $"Duration: {float stopwatch.ElapsedMilliseconds / 1000.0} seconds"
