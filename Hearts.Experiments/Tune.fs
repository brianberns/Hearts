namespace Hearts.Experiments

open System.Diagnostics

open Hearts
open Hearts.Heuristic

/// Variations of Killer Hearts V8, each against three of the current V8, by game wins under arena rules.
module Tune =

    /// Killer Hearts V8 with the given shift in the strength it needs to Shoot.
    let shifted shift = Killer.createWith shift

    /// How far the information set's player trails the game's low score.
    let private deficit = Killer.deficit

    /// The variations, by name.
    let variations =
        [
            "no shift", shifted (fun _ -> 0)   // V8 before trailingShootShift
            "shift -4", shifted (fun _ -> -4)
            "shift -2", shifted (fun _ -> -2)
            "shift +2", shifted (fun _ -> 2)
            "shift +4", shifted (fun _ -> 4)
            "-3 trailing 20+", shifted (fun i -> if deficit i >= 20 then -3 else 0)
            "-6 trailing 20+", shifted (fun i -> if deficit i >= 20 then -6 else 0)
            "+3 leading", shifted (fun i -> if deficit i = 0 then 3 else 0)
            "-3 trailing 20+, +3 leading", shifted (fun i -> if deficit i >= 20 then -3 elif deficit i = 0 then 3 else 0)
            for shift in [ -9; -12 ] do
                $"{shift} trailing 20+", shifted (fun i -> if deficit i >= 20 then shift else 0)
            for trail in [ 1; 5; 10; 30 ] do
                $"-6 trailing {trail}+", shifted (fun i -> if deficit i >= trail then -6 else 0)
            for shift in [ -4; -8 ] do
                $"{shift} trailing 10+", shifted (fun i -> if deficit i >= 10 then shift else 0)
        ]

    /// Runs the given variations (all, if none are named), each against three of the current V8, on the deals
    /// of the given seed.
    let run rngSeed numGames (names : string list) =
        let report name champion challenger =
            let watch = Stopwatch.StartNew()
            let rate = Tournament.run rngSeed numGames champion challenger
            printfn $"%-36s{name} %.4f{rate}   (%.0f{watch.Elapsed.TotalMinutes} min)"
        printfn $"{numGames} games each, seed {rngSeed}, duplicate deals, arena rules (0.25 is even)"
        for name, player in variations do
            if names.IsEmpty || List.contains name names then
                report $"{name} vs three V8:" Killer.player player
