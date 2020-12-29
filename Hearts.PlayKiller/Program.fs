namespace Hearts.PlayKiller

open System

open PlayingCards
open Hearts

module Killer =

    /// Initializes communication with Killer Hearts.
    let startSesssion () =
        let record = SharedRecord.readSessionStart ()
        SharedRecord.writeGeneral ClientRecordType.SessionStart
        record

    /// Starts a new game with KH.
    let startGame () =
        let record = SharedRecord.readGameStart ()
        SharedRecord.writeGeneral ClientRecordType.GameStart
        record

    /// Starts a new deal with KH.
    let startDeal () =
        let record = SharedRecord.readDealStart ()
        SharedRecord.writeGeneral ClientRecordType.DealStart
        record

    /// Receives each player's hand from KH.
    let receiveHands () =

        /// Gets a hand from KH.
        let receiveHand _ =
            let record = SharedRecord.readHand ()
            assert(record.Cards.Count = ClosedDeal.numCardsPerHand)
            SharedRecord.writeGeneral ClientRecordType.Hand
            record

            // arrange hands by seat
        Seq.init Seat.numSeats receiveHand
            |> Seq.map (fun record ->
                record.Seat, record.Cards)
            |> Map

    let receivePass deal =
        let record = SharedRecord.readExchangeOutgoing ()
        if record.Seat <> (deal |> OpenDeal.currentPlayer) then failwith "Unexpected"
        if record.Cards.Count <> Exchange.numCards then failwith "Unexpected"
        SharedRecord.writeGeneral ClientRecordType.ExchangeOutgoing
        record.Cards

    let sendPass deal score player =
        let cards = player.MakePass deal score
        let record = SharedRecord.readExchangeOutgoing ()
        if record.Seat <> (deal |> OpenDeal.currentPlayer) then failwith "Unexpected"
        if record.Cards.Count <> 0 then failwith "Unexpected"
        SharedRecord.writeExchangeOutgoing cards
        cards

    /// Ignores unused messages from KH.
    let sync deal =

            // ignore incoming passes
        if deal.Exchange |> Exchange.isComplete
            && deal.ClosedDeal |> ClosedDeal.numCardsPlayed = 0 then
            for _ = 1 to Seat.numSeats do
                SharedRecord.readExchangeIncoming () |> ignore
                SharedRecord.writeGeneral ClientRecordType.ExchangeIncoming

            // at the beginning of a trick?
        if deal.Exchange |> Exchange.isComplete
            && deal.ClosedDeal.CurrentTrickOpt.Value.Cards.Length = 0 then

                // ignore previous trick end
            if deal.ClosedDeal.CompletedTricks.Length > 0 then
                SharedRecord.readGeneral ServerRecordType.TrickEnd
                SharedRecord.writeGeneral ClientRecordType.TrickEnd

                // ignore new trick start
            SharedRecord.readTrickStart () |> ignore
            SharedRecord.writeGeneral ClientRecordType.TrickStart

    let receivePlay deal =
        sync deal
        let record = SharedRecord.readPlay ()
        if record.Seat <> (deal |> OpenDeal.currentPlayer) then failwith "Unexpected"
        SharedRecord.writeGeneral ClientRecordType.Play
        record.Cards |> Seq.exactlyOne

    let sendPlay deal score player =
        sync deal
        let card = player.MakePlay deal score
        let record = SharedRecord.readPlay ()
        if record.Seat <> (deal |> OpenDeal.currentPlayer) then failwith "Unexpected"
        if record.Cards.Count <> 0 then failwith "Unexpected"
        SharedRecord.writePlay card
        card

    let serverPlayer =

        let makePass deal score =
            receivePass deal

        let makePlay deal score =
            receivePlay deal

        {
            MakePass = makePass
            MakePlay = makePlay
        }

    let createClientPlayer player =

        let makePass deal score =
            sendPass deal score player

        let makePlay deal score =
            sendPlay deal score player

        {
            MakePass = makePass
            MakePlay = makePlay
        }

    let validateScore (scoreA : Score) (scoreB : Score) =
        assert(scoreA = scoreB)

    let playDeal (game : Game) dealer =
        let dealRec = startDeal ()
        validateScore game.Score dealRec.GameScore
        let dir = dealRec.ExchangeDirection
        let handMap = receiveHands ()
        let deal = OpenDeal.fromHands dealer dir handMap
        let game = { game with CurrentDealOpt = Some deal }
        game |> Game.playDeal

    let playGame game =

        let rec loop game dealer =

                // play a deal
            let game = playDeal game dealer

                // ignore final trick end
            SharedRecord.readGeneral ServerRecordType.TrickEnd
            SharedRecord.writeGeneral ClientRecordType.TrickEnd

            loop game dealer.Next

        let record = startGame ()
        loop game record.Dealer

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

    let run player nGames =

        let sessionRec = Killer.startSesssion ()

        let playerMap =
            let clientPlayer =
                Killer.createClientPlayer player
            Seat.allSeats
                |> Seq.map (fun seat ->
                    let protocolPlayer =
                        if sessionRec.ClientSeats.Contains(seat) then
                            clientPlayer
                        else
                            Killer.serverPlayer
                    seat, protocolPlayer)
                |> Map

        for gameNum = 1 to nGames do
            Game.create playerMap
                |> Killer.playGame
                |> ignore

    [<EntryPoint>]
    let main argv =
        try
            run Random.player 1000
        with ex ->
            printfn "%s" ex.Message
        0
