namespace Hearts.Heuristic

open System

open Hearts

module Program =

    let rng = Random(0)

    let randomPlayer =
        let act (infoSet : InformationSet) =
            rng.GetItems(infoSet.LegalActions, 1)
                |> Array.exactlyOne
        { Act = act }

    let eval player =
        Tournament.run rng true 200000 randomPlayer player
            |> snd

    printfn $"{eval Claude.player}"
