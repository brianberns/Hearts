namespace Hearts.PlayKiller

open System
open Hearts

module Random =

    let rng = Random(0)

    let act infoSet =
        let card =
            infoSet.LegalActions
                |> Array.randomChoiceWith rng
        infoSet.LegalActionType, card

    let player = { Act = act }

module Program =

    [<EntryPoint>]
    let main argv =
        try
            let score = Killer.run Random.player
            for (KeyValue(seat, points)) in score.ScoreMap do
                printfn "%A: %d" seat points
        with ex ->
            printfn "%s" ex.Message
        0
