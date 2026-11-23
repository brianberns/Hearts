namespace Hearts.Experiments

open System.Diagnostics

open Hearts
open Hearts.Heuristic
open Hearts.Model

/// The deterministic model player (best equivalence class) against the non-deterministic one (sampling
/// from the strategy), with the same model, by game wins under arena rules.
module Determinism =

    /// Runs the experiments.
    let run modelPath numGames =
        use model = Players.loadModel modelPath
        let det, rand = Strategy.createPlayer model, Strategy.createRandomPlayer model
        let report name champion challenger =
            let watch = Stopwatch.StartNew()
            let rate = Tournament.run 0 numGames champion challenger
            printfn $"%-44s{name} %.4f{rate}   (%.0f{watch.Elapsed.TotalMinutes} min)"
        printfn $"{System.IO.Path.GetFileName modelPath}, {numGames} games each, duplicate deals, arena rules (0.25 is even)"
        report "deterministic vs three sampling:" rand det
        report "sampling vs three deterministic:" det rand
        report "deterministic vs three Claude:" Claude.player det
        report "sampling vs three Claude:" Claude.player rand
