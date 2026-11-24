namespace Hearts.Experiments

open System.Diagnostics

open Hearts
open Hearts.Heuristic
open Hearts.Model

/// The benchmark for model training, by game wins under arena rules: the given model against three Killer
/// Hearts V8.
module Benchmark =

    /// Runs the benchmark on the deals of the given seed.
    let run modelPath rngSeed numGames =
        use model = Players.loadModel modelPath
        let report name makeChampion makeChallenger =
            let watch = Stopwatch.StartNew()
            let rate = Tournament.runSeeded rngSeed numGames makeChampion makeChallenger
            printfn $"%-36s{name} %.4f{rate}   (%.0f{watch.Elapsed.TotalMinutes} min)"
        printfn $"{System.IO.Path.GetFileName modelPath}, {numGames} games each, seed {rngSeed}, duplicate deals, arena rules (0.25 is even)"
        report "model vs three Killer Hearts V8:" (fun _ -> Killer.player) (Strategy.createPlayerSeeded model)
