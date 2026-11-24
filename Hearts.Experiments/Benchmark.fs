namespace Hearts.Experiments

open System.Diagnostics

open Hearts
open Hearts.Heuristic
open Hearts.Model

/// The benchmarks for model training, by game wins under arena rules: Claude against three Killer Hearts,
/// and the given model against three Claude.
module Benchmark =

    /// Runs the benchmarks on the deals of the given seed.
    let run modelPath rngSeed numGames =
        use model = Players.loadModel modelPath
        let report name makeChampion makeChallenger =
            let watch = Stopwatch.StartNew()
            let rate = Tournament.runSeeded rngSeed numGames makeChampion makeChallenger
            printfn $"%-36s{name} %.4f{rate}   (%.0f{watch.Elapsed.TotalMinutes} min)"
        printfn $"{System.IO.Path.GetFileName modelPath}, {numGames} games each, seed {rngSeed}, duplicate deals, arena rules (0.25 is even)"
        report "Claude vs three Killer Hearts:" (fun _ -> Tune.killerHearts) (fun _ -> Claude.player)
        report "model vs three Claude:" (fun _ -> Claude.player) (Strategy.createPlayerSeeded model)
