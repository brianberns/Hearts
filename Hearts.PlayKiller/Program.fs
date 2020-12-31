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

    let validateScore (scoreA : Score) (scoreB : Score) =
        assert(scoreA = scoreB)

    let run player =

        let session =
            let record = Killer.startSession ()
            let wrappedPlayer = Killer.wrap player
            Seat.allSeats
                |> Seq.map (fun seat ->
                    let khPlayer =
                        if record.ClientSeats.Contains(seat) then
                            wrappedPlayer
                        else
                            Killer.player
                    seat, khPlayer)
                |> Map
                |> Session

        session.GameStartEvent.Add(fun dealer ->
            let record = Killer.startGame ()
            assert(dealer = record.Dealer))

        session.ExchangeFinishEvent.Add(fun () ->
            for _ = 1 to Seat.numSeats do
                Killer.receiveExchangeIncoming () |> ignore)

        session.DealStartEvent.Add(fun (dir, gameScore) ->
            if gameScore = Score.zero then   // KH bug: deal start only occurs on first deal of hand
                let record = Killer.startDeal ()
                assert(dir = record.ExchangeDirection)
                assert(gameScore = record.GameScore))

        session.TrickStartEvent.Add(fun leader ->
            let record = Killer.startTrick ()
            assert(leader = record.Leader))

        session.TrickFinishEvent.Add(fun () ->
            Killer.finishTrick () |> ignore)

        session.EarlyFinalizationEvent.Add(fun _ ->
            Killer.startTrick () |> ignore)   // ignore bogus new trick started by KH

        session.DealFinishEvent.Add(fun _ ->
            Killer.finishDeal ())

        let createDeal dealer dir =
            let handMap = Killer.receiveHands ()
            OpenDeal.fromHands dealer dir handMap

        session.Run(Seat.South, ExchangeDirection.Left, createDeal)

    [<EntryPoint>]
    let main argv =
        try
            run Random.player
        with ex ->
            printfn "%s" ex.Message
        0
