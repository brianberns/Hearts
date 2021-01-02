namespace Hearts.PlayKiller

open PlayingCards
open Hearts

type State =
    {
        PlayerMap : Map<Seat, Player>
        DealerOpt : Option<Seat>
        GameScoreOpt : Option<Score>
        ExchangeDirectionOpt : Option<ExchangeDirection>
        HandMap : Map<Seat, Set<Card>>
        DealOpt : Option<OpenDeal>
        SessionScoreOpt : Option<Score>
    }

module State =

    let initial =
        {
            PlayerMap = Map.empty
            DealerOpt = None
            GameScoreOpt = None
            ExchangeDirectionOpt = None
            HandMap = Map.empty
            DealOpt = None
            SessionScoreOpt = None
        }

module Killer =

    let sessionStart state player (record : SessionStartRecord) =
        Protocol.writeEmpty ClientRecordType.SessionStart
        {
            state with
                PlayerMap =
                    record.ClientSeats
                        |> Seq.map (fun seat ->
                            seat, player)
                        |> Map
        }

    let gameStart state (record : GameStartRecord) =
        Protocol.writeEmpty ClientRecordType.GameStart
        {
            state with
                DealerOpt = Some record.Dealer
                // NumGames = record.NumGames
        }

    let dealStart state (record : DealStartRecord) =
        Protocol.writeEmpty ClientRecordType.DealStart
        {
            state with
                GameScoreOpt = Some record.GameScore
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
                            DealerOpt = None
                            ExchangeDirectionOpt = None
                            HandMap = Map.empty
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
        match state.DealOpt, state.GameScoreOpt with
            | Some deal, Some gameScore ->
                let cards =
                    if record.Cards.IsEmpty then
                        let cards =
                            let player = state.PlayerMap.[record.Seat]
                            player.MakePass deal gameScore
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
            | _ -> failwith "Unexpected"

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

    let play state (record : SeatCardsRecord) =
        match state.DealOpt, state.GameScoreOpt with
            | Some deal, Some gameScore ->
                let card =
                    if record.Cards.IsEmpty then
                        let card =
                            let player = state.PlayerMap.[record.Seat]
                            player.MakePlay deal gameScore
                        Protocol.writePlay card
                        card
                    else
                        Protocol.writeEmpty ClientRecordType.Play
                        record.Cards |> Seq.exactlyOne
                {
                    state with
                        DealOpt =
                            deal |> OpenDeal.addPlay card |> Some
                }
            | _ -> failwith "Unexpected"

    let trickFinish state =
        Protocol.writeEmpty ClientRecordType.TrickFinish
        assert(
            (state.DealOpt.Value.ClosedDeal
                |> ClosedDeal.numCardsPlayed) % Seat.numSeats = 0)
        state : State

    let dealFinish state =
        Protocol.writeEmpty ClientRecordType.DealFinish
        match state.DealOpt with
            | Some deal ->
                {
                    state with
                        DealerOpt = Some deal.Exchange.Dealer.Next
                        ExchangeDirectionOpt =
                            deal.Exchange.ExchangeDirection
                                |> ExchangeDirection.next
                                |> Some
                        DealOpt = None
                }
            | None -> failwith "Unexpected"

    let gameFinish state (record : GameFinishRecord) =
        Protocol.writeEmpty ClientRecordType.GameFinish
        assert(state.DealOpt.IsNone)
        {
            state with
                GameScoreOpt = Some record.GameScore
        }

    let sessionFinish (state : State) (record : SessionFinishRecord) =
        {
            state with
                SessionScoreOpt = Some record.SessionScore
        }

    let advance state player =
        match Protocol.read () with
            | SessionStart record ->
                sessionStart state player record
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
            | Play record ->
                play state record
            | TrickFinish ->
                trickFinish state
            | DealFinish ->
                dealFinish state
            | GameFinish record ->
                gameFinish state record
            | SessionFinish record ->
                sessionFinish state record

    let run player =

        let rec loop state =
            let state = advance state player
            if state.SessionScoreOpt.IsNone then
                loop state

        loop State.initial
