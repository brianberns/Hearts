namespace Hearts.PlayKiller

open Hearts

module Killer =

    /// Initializes communication with Killer Hearts.
    let startSesssion () =
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

    /// Ignores unused messages from KH.
    let sync deal =

            // ignore incoming passes
        if deal.Exchange |> Exchange.isComplete
            && deal.ClosedDeal |> ClosedDeal.numCardsPlayed = 0 then
            for _ = 1 to Seat.numSeats do
                Protocol.readExchangeIncoming () |> ignore
                Protocol.writeEmpty ClientRecordType.ExchangeIncoming

            // at the beginning of a trick?
        if deal.Exchange |> Exchange.isComplete
            && deal.ClosedDeal.CurrentTrickOpt.Value.Cards.Length = 0 then

                // ignore previous trick end
            if deal.ClosedDeal.CompletedTricks.Length > 0 then
                Protocol.readIgnore ServerRecordType.TrickEnd
                Protocol.writeEmpty ClientRecordType.TrickEnd

                // ignore new trick start
            Protocol.readTrickStart () |> ignore
            Protocol.writeEmpty ClientRecordType.TrickStart

    /// Recieves a played card from KH.
    let receivePlay deal =
        sync deal
        let record = Protocol.readPlay ()
        if record.Seat <> (deal |> OpenDeal.currentPlayer) then failwith "Unexpected"
        Protocol.writeEmpty ClientRecordType.Play
        record.Cards |> Seq.exactlyOne

    /// Sends a played card to KH.
    let sendPlay deal score player =
        sync deal
        let card = player.MakePlay deal score
        let record = Protocol.readPlay ()
        if record.Seat <> (deal |> OpenDeal.currentPlayer) then failwith "Unexpected"
        if record.Cards.Count <> 0 then failwith "Unexpected"
        Protocol.writePlay card
        card
