namespace Hearts.PlayKiller

open PlayingCards
open Hearts

/// State maintained by the Killer Hearts client.
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

    /// Initial state.
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

    /// A session has started.
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

    /// A game has started.
    let gameStart state (record : GameStartRecord) =
        Protocol.writeEmpty ClientRecordType.GameStart
        {
            state with
                DealerOpt = Some record.Dealer
                // NumGames = record.NumGames
        }

    /// A deal has started.
    let dealStart state (record : DealStartRecord) =
        Protocol.writeEmpty ClientRecordType.DealStart
        {
            state with
                GameScoreOpt = Some record.GameScore
                ExchangeDirectionOpt = Some record.ExchangeDirection
        }

    /// A hand has been dealt.
    let hand state (record : SeatCardsRecord) =
        Protocol.writeEmpty ClientRecordType.Hand
        let handMap =
            state.HandMap
                |> Map.add record.Seat record.Cards

            // all cards have been dealt?
        if handMap.Count = Seat.numSeats then
            match state.DealerOpt, state.ExchangeDirectionOpt with
                | Some dealer, Some dir ->
                    {
                        state with
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

    /// Gets the player's full set of passed cards.
    let private getPass seat deal (hand : Hand) player =
        ((hand, Set.empty), [1 .. Pass.numCards])
            ||> Seq.fold (fun (hand, pass) _ ->
                let card =
                    InformationSet.create
                        seat hand (Some pass) None deal
                        |> player.Act
                assert(hand.Contains(card))
                hand.Remove(card),
                pass.Add(card))
            |> snd

    /// Cards are being passed.
    let passOutgoing state (record : SeatCardsRecord) =
        match state.DealOpt with
            | Some deal ->
                let cards =

                        // we are passing
                    if record.Cards.IsEmpty then
                        let cards =
                            getPass
                                record.Seat
                                deal.ClosedDeal
                                (OpenDeal.currentHand deal)
                                state.PlayerMap[record.Seat]
                        Protocol.writePassOutgoing cards
                        cards

                        // server is passing
                    else
                        Protocol.writeEmpty ClientRecordType.PassOutgoing
                        record.Cards

                    // add pass to deal
                let deal =
                    (deal, cards)
                        ||> Seq.fold (fun deal card ->
                            OpenDeal.addPass card deal)
                {
                    state with
                        DealOpt = Some deal
                }
                
            | _ -> failwith "Unexpected"

    /// Cards are being received. (These messages are redundant and
    /// we just ignore them.)
    let passIncoming state (_ : SeatCardsRecord) =
        Protocol.writeEmpty ClientRecordType.PassIncoming
        state : State

    /// A trick has started.
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

    /// A card is being played.
    let play state (record : SeatCardsRecord) =
        match state.DealOpt with
            | Some deal ->

                let card =

                        // we are playing
                    if record.Cards.IsEmpty then
                        let card =
                            let player = state.PlayerMap[record.Seat]
                            let infoSet =
                                let outPassOpt =
                                    deal.ExchangeOpt
                                        |> Option.map _.PassMap[record.Seat]
                                let inPassOpt =
                                    let seat =
                                        deal.ClosedDeal.ExchangeDirection
                                            |> ExchangeDirection.unapply record.Seat
                                    deal.ExchangeOpt
                                        |> Option.map _.PassMap[seat]
                                InformationSet.create
                                    record.Seat
                                    (OpenDeal.currentHand deal)
                                    outPassOpt
                                    inPassOpt
                                    deal.ClosedDeal
                            player.Act infoSet
                        Protocol.writePlay card
                        card

                        // server is passing
                    else
                        Protocol.writeEmpty ClientRecordType.Play
                        record.Cards |> Seq.exactlyOne

                {
                    state with
                        DealOpt =
                            deal |> OpenDeal.addPlay card |> Some
                }

            | _ -> failwith "Unexpected"

    /// A trick has finished.
    let trickFinish state =
        Protocol.writeEmpty ClientRecordType.TrickFinish
        assert(
            (state.DealOpt.Value.ClosedDeal
                |> ClosedDeal.numCardsPlayed) % Seat.numSeats = 0)
        state : State

    /// A deal has finished.
    let dealFinish state =
        Protocol.writeEmpty ClientRecordType.DealFinish
        match state.DealOpt with
            | Some deal ->
                {
                        // prepare for next deal
                    state with
                        DealerOpt =
                            deal.ClosedDeal.Dealer
                                |> Seat.next
                                |> Some
                        ExchangeDirectionOpt =
                            deal.ClosedDeal.ExchangeDirection
                                |> ExchangeDirection.next
                                |> Some
                        DealOpt = None
                }
            | None -> failwith "Unexpected"

    /// A game has finished.
    let gameFinish state (record : GameFinishRecord) =
        Protocol.writeEmpty ClientRecordType.GameFinish
        assert(state.DealOpt.IsNone)
        {
            state with
                GameScoreOpt = Some record.GameScore
        }

    /// A session has finished.
    let sessionFinish (state : State) (record : SessionFinishRecord) =
        {
            state with
                SessionScoreOpt = Some record.SessionScore
        }

    /// Advances the session in response to the next record
    /// from KH.
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
            | PassOutgoing record ->
                passOutgoing state record
            | PassIncoming record ->
                passIncoming state record
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

    /// Runs a KH session with the given client player.
    let run player =

        let rec loop state =
            let state = advance state player
            match state.SessionScoreOpt with
                | None -> loop state
                | Some score -> score

        loop State.initial
