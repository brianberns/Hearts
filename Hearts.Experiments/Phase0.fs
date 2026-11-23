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
        let prod, weak = Strategy.createPlayer production, Strategy.createPlayer weakened
        let report name champion challenger =
            let watch = Stopwatch.StartNew()
            let rate = Tournament.run 0 true numGames champion challenger
            printfn $"%-48s{name} %.4f{rate}   (%.0f{watch.Elapsed.TotalMinutes} min)"
        printfn $"{numGames} games each, duplicate deals, arena rules (0.25 is even)"
        report "production vs three weakened:" weak prod
        report "weakened vs three production:" prod weak
        report "production vs three Claude:" Claude.player prod
        report "weakened vs three Claude:" Claude.player weak
        report "guarded production vs three Claude:" Claude.player (Players.guarded prod)
        report "guarded weakened vs three Claude:" Claude.player (Players.guarded weak)
