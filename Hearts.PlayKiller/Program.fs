namespace Hearts.PlayKiller

open System

open PlayingCards
open Hearts

module Random =

    let rng = Random(0)

    let makePass deal _ =
        deal
            |> OpenDeal.currentHand
            |> Seq.toArray
            |> Array.shuffle rng
            |> Seq.take Exchange.numCards
            |> set

    let makePlay deal _ =
        let hand = OpenDeal.currentHand deal
        deal.ClosedDeal
            |> ClosedDeal.legalPlays hand
            |> Seq.toArray
            |> Array.shuffle rng
            |> Seq.head

    let player =
        {
            MakePass = makePass
            MakePlay = makePlay
        }

module Program =

    [<EntryPoint>]
    let main argv =
        try
            let (ScoreMap scoreMap) = Killer.run Random.player
            for (KeyValue(seat, points)) in scoreMap do
                printfn "%A: %d" seat points
        with ex ->
            printfn "%s" ex.Message
        0
