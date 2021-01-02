namespace Hearts.PlayKiller

open PlayingCards
open Hearts

type State =
    {
        PlayerMap : Map<Seat, Player>
        DealerOpt : Option<Seat>
        NumGames : int
        GameScore : Score
        ExchangeDirectionOpt : Option<ExchangeDirection>
        HandMap : Map<Seat, Set<Card>>
        DealOpt : Option<OpenDeal>
    }

module State =

    let initial =
        {
            PlayerMap = Map.empty
            DealerOpt = None
            NumGames = 0
            GameScore = Score.zero
            ExchangeDirectionOpt = None
            HandMap = Map.empty
            DealOpt = None
        }

module Killer =

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

    /// KHearts server player.
    let serverPlayer =
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

    let sessionStart state rawPlayer (record : SessionStartRecord) =
        Protocol.writeEmpty ClientRecordType.SessionStart
        {
            state with
                PlayerMap =
                    let clientPlayer = wrap rawPlayer
                    Seat.allSeats
                        |> Seq.map (fun seat ->
                            let player =
                                if record.ClientSeats.Contains(seat) then
                                    clientPlayer
                                else
                                    serverPlayer
                            seat, player)
                        |> Map
        }

    let gameStart state (record : GameStartRecord) =
        Protocol.writeEmpty ClientRecordType.GameStart
        {
            state with
                DealerOpt = Some record.Dealer
                NumGames = record.NumGames
        }

    let dealStart state (record : DealStartRecord) =
        Protocol.writeEmpty ClientRecordType.DealStart
        {
            state with
                State.GameScore = record.GameScore
                ExchangeDirectionOpt = Some record.ExchangeDirection
        }

    let hand state (record : SeatCardsRecord) =
        Protocol.writeEmpty ClientRecordType.Hand
        let handMap =
            state.HandMap
                |> Map.add record.Seat record.Cards
        if handMap.Count = Seat.numSeats then
            match state.DealerOpt, state.ExchangeDirectionOpt with
                | Some dealer, Some dir ->
                    {
                        state with
                            HandMap = Map.empty
                            DealerOpt = None
                            ExchangeDirectionOpt = None
                            DealOpt =
                                OpenDeal.fromHands dealer dir handMap
                                    |> Some
                    }
                | _ -> failwith "Unexpected"
            else
                assert(state.DealOpt.IsNone)
                {
                    state with
                        HandMap = handMap
                }

    let exchangeOutgoing state (record : SeatCardsRecord) =
        match state.DealOpt with
            | Some deal ->
                let cards =
                    if record.Cards.IsEmpty then
                        let cards =
                            let player = state.PlayerMap.[record.Seat]
                            player.MakePass deal state.GameScore
                        Protocol.writeExchangeOutgoing cards
                        cards
                    else
                        Protocol.writeEmpty ClientRecordType.ExchangeOutgoing
                        record.Cards
                {
                    state with
                        DealOpt =
                            deal |> OpenDeal.addPass cards |> Some
                }
            | None -> failwith "Unexpected"

    let exchangeIncoming state (_ : SeatCardsRecord) =
        Protocol.writeEmpty ClientRecordType.ExchangeIncoming
        state : State

    let trickStart state (record : TrickStartRecord) =
        Protocol.writeEmpty ClientRecordType.TrickStart
        let deal =
            match state.DealOpt with
                | Some deal ->
                    let deal =
                        if deal.ClosedDeal.CurrentTrickOpt.IsNone then
                            deal |> OpenDeal.startPlay
                        else deal
                    assert
                        (deal.ClosedDeal.CurrentTrickOpt.Value.Leader
                            = record.Leader)
                    assert
                        (deal.ClosedDeal.CurrentTrickOpt.Value.Cards.IsEmpty)
                    assert
                        (deal.ClosedDeal.CompletedTricks.Length + 1
                            = record.TrickNum)
                    deal
                | None -> failwith "Unexepcted"
        {
            state with
                DealOpt = Some deal
        }

    let advance state rawPlayer =
        match Protocol.read () with
            | SessionStart record ->
                sessionStart state rawPlayer record
            | GameStart record ->
                gameStart state record
            | DealStart record ->
                dealStart state record
            | Hand record ->
                hand state record
            | ExchangeOutgoing record ->
                exchangeOutgoing state record
            | ExchangeIncoming record ->
                exchangeIncoming state record
            | TrickStart record ->
                trickStart state record

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
