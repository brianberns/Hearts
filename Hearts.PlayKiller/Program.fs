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

    let serverPlayer =

        let makePass deal score =
            Killer.receivePass deal

        let makePlay deal score =
            Killer.receivePlay deal

        {
            MakePass = makePass
            MakePlay = makePlay
        }

    let createClientPlayer player =

        let makePass deal score =
            Killer.sendPass deal score player

        let makePlay deal score =
            Killer.sendPlay deal score player

        {
            MakePass = makePass
            MakePlay = makePlay
        }

    let validateScore (scoreA : Score) (scoreB : Score) =
        assert(scoreA = scoreB)

    let playDeal (game : Game) dealer =
        let dealRec = Killer.startDeal ()
        validateScore game.Score dealRec.GameScore
        let dir = dealRec.ExchangeDirection
        let handMap = Killer.receiveHands ()
        let deal = OpenDeal.fromHands dealer dir handMap
        let game = { game with CurrentDealOpt = Some deal }
        game |> Game.playDeal

    let playGame game =

        let rec loop game dealer =

                // play a deal
            let game = playDeal game dealer

                // ignore final trick end
            Protocol.readIgnore ServerRecordType.TrickEnd
            Protocol.writeEmpty ClientRecordType.TrickEnd

            loop game dealer.Next

        let record = Killer.startGame ()
        loop game record.Dealer

    let run player nGames =

        let sessionRec = Killer.startSesssion ()

        let playerMap =
            let clientPlayer =
                createClientPlayer player
            Seat.allSeats
                |> Seq.map (fun seat ->
                    let protocolPlayer =
                        if sessionRec.ClientSeats.Contains(seat) then
                            clientPlayer
                        else
                            serverPlayer
                    seat, protocolPlayer)
                |> Map

        for gameNum = 1 to nGames do
            Game.create playerMap
                |> playGame
                |> ignore

    [<EntryPoint>]
    let main argv =
        try
            run Random.player 1000
        with ex ->
            printfn "%s" ex.Message
        0
