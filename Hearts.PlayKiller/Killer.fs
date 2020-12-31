namespace Hearts.PlayKiller

open Hearts

module Killer =

    /// Initializes communication with Killer Hearts.
    let startSession () =
        let record = Protocol.readSessionStart ()
        Protocol.writeEmpty ClientRecordType.SessionStart
        record

    /// Starts a new game with KH.
    let startGame () =
        let record = Protocol.readGameStart ()
        Protocol.writeEmpty ClientRecordType.GameStart
        record

    /// Starts a new deal with KH.
    let startDeal () =
        let record = Protocol.readDealStart ()
        Protocol.writeEmpty ClientRecordType.DealStart
        record

    /// Receives each player's hand from KH.
    let receiveHands () =

        /// Gets a hand from KH.
        let receiveHand _ =
            let record = Protocol.readHand ()
            assert(record.Cards.Count = ClosedDeal.numCardsPerHand)
            Protocol.writeEmpty ClientRecordType.Hand
            record

            // arrange hands by seat
        Seq.init Seat.numSeats receiveHand
            |> Seq.map (fun record ->
                record.Seat, record.Cards)
            |> Map

    /// Receives passed cards from KH.
    let receivePass deal =
        let record = Protocol.readExchangeOutgoing ()
        if record.Seat <> (deal |> OpenDeal.currentPlayer) then failwith "Unexpected"
        if record.Cards.Count <> Exchange.numCards then failwith "Unexpected"
        Protocol.writeEmpty ClientRecordType.ExchangeOutgoing
        record.Cards

    /// Sends passed cards to KH.
    let sendPass deal score player =
        let cards = player.MakePass deal score
        let record = Protocol.readExchangeOutgoing ()
        if record.Seat <> (deal |> OpenDeal.currentPlayer) then failwith "Unexpected"
        if record.Cards.Count <> 0 then failwith "Unexpected"
        Protocol.writeExchangeOutgoing cards
        cards

    /// Receives an incoming pass from KH. Can be safely ignored.
    let receiveExchangeIncoming () =
        let record = Protocol.readExchangeIncoming ()
        Protocol.writeEmpty ClientRecordType.ExchangeIncoming
        record

    /// Starts a new trick with KH.
    let startTrick () =
        let record = Protocol.readTrickStart ()
        Protocol.writeEmpty ClientRecordType.TrickStart
        record

    /// Recieves a played card from KH.
    let receivePlay deal =
        let record = Protocol.readPlay ()
        if record.Seat <> (deal |> OpenDeal.currentPlayer) then failwith "Unexpected"
        Protocol.writeEmpty ClientRecordType.Play
        record.Cards |> Seq.exactlyOne

    /// Sends a played card to KH.
    let sendPlay deal score player =
        let card = player.MakePlay deal score
        let record = Protocol.readPlay ()
        if record.Seat <> (deal |> OpenDeal.currentPlayer) then failwith "Unexpected"
        if record.Cards.Count <> 0 then failwith "Unexpected"
        Protocol.writePlay card
        card

    /// Finishes a trick with KH.
    let finishTrick () =
        Protocol.readIgnore ServerRecordType.TrickFinish
        Protocol.writeEmpty ClientRecordType.TrickFinish

    /// Finishes a deal with KH.
    let finishDeal () =
        Protocol.readIgnore ServerRecordType.DealFinish
        Protocol.writeEmpty ClientRecordType.DealFinish

    /// Finishes a game with KH.
    let finishGame () =
        let record = Protocol.readGameFinish ()
        Protocol.writeEmpty ClientRecordType.GameFinish
        record

    /// KHearts player.
    let player =
        {
            MakePass = fun deal _ -> receivePass deal
            MakePlay = fun deal _ -> receivePlay deal
        }

    /// Wraps the given player for use with KHearts.
    let wrap player =
        {
            MakePass =
                fun deal score -> sendPass deal score player
            MakePlay =
                fun deal score -> sendPlay deal score player
        }
