namespace Hearts.Heuristic

open System

open Hearts

module Program =

    let rng = Random(0)

    let eval player =
        Tournament.run rng true 200000 Claude.player player
            |> snd

    printfn $"{eval Claude.player}"
