namespace Hearts.Heuristic

open System

open Hearts
open Hearts.Learn

module Program =

    let rng = Random(42)  // Fixed seed for deterministic evaluation

    let eval player =
        Tournament.run rng true 20000 Trickster.player player
            |> snd

    printfn $"{eval Claude.player}"
