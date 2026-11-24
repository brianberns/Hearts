namespace Hearts.Experiments

open System.Diagnostics

open Hearts
open Hearts.Heuristic
open Hearts.Model

/// Phase 0: how the production and weakened models fare by game wins under arena rules, against each other
/// and against Claude, with and without Claude's guard against losing Shoots.
module Phase0 =

    /// Runs the experiments.
    let run productionPath weakenedPath numGames =
        use production = Players.loadModel productionPath
        use weakened = Players.loadModel weakenedPath
        let prod, weak = Strategy.createPlayerSeeded production, Strategy.createPlayerSeeded weakened
        let claude _ = Claude.player
        let guarded makePlayer seed = Players.guarded (makePlayer seed)
        let report name makeChampion makeChallenger =
            let watch = Stopwatch.StartNew()
            let rate = Tournament.runSeeded 0 numGames makeChampion makeChallenger
            printfn $"%-48s{name} %.4f{rate}   (%.0f{watch.Elapsed.TotalMinutes} min)"
        printfn $"{numGames} games each, duplicate deals, arena rules (0.25 is even)"
        report "production vs three weakened:" weak prod
        report "weakened vs three production:" prod weak
        report "production vs three Claude:" claude prod
        report "weakened vs three Claude:" claude weak
        report "guarded production vs three Claude:" claude (guarded prod)
        report "guarded weakened vs three Claude:" claude (guarded weak)
