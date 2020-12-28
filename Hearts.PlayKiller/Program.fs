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
        let record = SharedRecord.readNewDeal ()
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

    (*
    let exchange clientSeats =

        for i = 1 to 4 do
            let record =
                SharedRecord.readExchangeOutgoing clientSeats
            record
                |> Option.iter (fun exchOutRec ->
                    assert(exchOutRec.Cards.Count = Exchange.numCards)
                    assert
                        (exchOutRec.Cards
                            |> Seq.forall (fun card ->
                                handMap.[exchOutRec.Seat].Contains(card))))
            SharedRecord.writeGeneral ClientRecordType.ExchangeOutgoing
    *)

    (*
    let receivePlay () =
        let record = SharedRecord.readPlay ()
        SharedRecord.writeGeneral ClientRecordType.Play
        record
    *)

    let serverPlayer =

        let makePass deal score =
            Set.empty // Killer.receivePass

        let makePlay deal score =
            Card.allCards.[0] // Killer.receivePlay

        {
            MakePass = makePass
            MakePlay = makePlay
        }

    let createClientPlayer player =

        let makePass deal score =
            Set.empty // Killer.sendPass score deal player

        let makePlay deal score =
            Card.allCards.[0] // Killer.sendPlay score deal player

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
        let gameRec = startGame ()
        playDeal game gameRec.Dealer

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
