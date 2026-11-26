namespace Hearts.Experiments

open Hearts
open Hearts.Heuristic

/// How much duplicate deals narrow the error bars on a win rate: the challenger plays each cycle's deals
/// from all four seats, against three champions, and the spread of its wins per cycle is compared with
/// the spread of independent games.
module Duplicate =

    /// The challenger's wins in one cycle: the same deals, with the challenger in each seat in turn.
    let private runCycle rngSeed (champion : Player) (challenger : Player) iCycle =
        let cycleSeed = SplitMixRandom.GetSeed(rngSeed, iCycle)
        Seat.cycle Seat.South
            |> Seq.sumBy (fun challengerSeat ->
                let playerMap =
                    Seat.allSeats
                        |> Seq.map (fun seat -> seat, if seat = challengerSeat then challenger else champion)
                        |> Map
                let rng = SplitMixRandom(cycleSeed)   // the same deals in every game of the cycle
                let winners =
                    Game.playGame Rules.arena rng playerMap Seat.South
                        |> GameScore.findGameWinners Rules.arena.TieGame
                if winners.Contains(challengerSeat) then 1 else 0)

    /// Reports the challenger's win rate and its 95% error bar with and without duplicate deals.
    let private report name rngSeed numCycles champion challenger =
        let wins = Array.Parallel.init numCycles (runCycle rngSeed champion challenger)
        let n = float (4 * numCycles)
        let p = float (Array.sum wins) / n
        let mean = Array.averageBy float wins
        let varCycle = (wins |> Array.sumBy (fun w -> (float w - mean) ** 2.0)) / float (numCycles - 1)
        let independent = 1.96 * sqrt (p * (1.0 - p) / n)      // n independent games
        let duplicate = 1.96 * sqrt (varCycle / float numCycles) / 4.0
        let dist = Array.init 5 (fun k -> wins |> Array.filter ((=) k) |> Array.length)
        printfn $"{name}: win rate %.4f{p} over {int n:N0} games"
        printfn $"   wins per cycle (0-4): %A{dist}"
        printfn $"   95%% error bar: independent ±%.4f{independent}, duplicate ±%.4f{duplicate} (%.2f{duplicate / independent}x, as if %.1f{(independent / duplicate) ** 2.0}x the games)"

    /// Runs the measurement for Gemini against Killer Hearts V8, both ways round.
    let run rngSeed numGames =
        let numCycles = numGames / 4
        report "Killer vs 3 Killer (check: one win per cycle)" rngSeed numCycles Killer.player Killer.player
        report "Killer shift +4 vs 3 Killer" rngSeed numCycles Killer.player (Tune.shifted (fun _ -> 4))
        report "Killer shift -4 vs 3 Killer" rngSeed numCycles Killer.player (Tune.shifted (fun _ -> -4))
        report "Gemini vs 3 Killer" rngSeed numCycles Killer.player Gemini.player
        report "Killer vs 3 Gemini" rngSeed numCycles Gemini.player Killer.player
