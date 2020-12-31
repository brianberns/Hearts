namespace Hearts

open System
open System.ComponentModel

open PlayingCards

type Player =
    {
        /// Function that passes cards in the given deal.
        MakePass : OpenDeal -> Score (*game score*) -> Set<Card>

        /// Function plays a card in the given deal.
        MakePlay : OpenDeal -> Score (*game score*) -> Card
    }

/// Manages a series of games with the given players.
type Session
    (playerMap : Map<_, _>,
    ?syncOpt : ISynchronizeInvoke) =

        // initialize events raised by this object
    let sessionStartEvent = Event<_>()
    let gameStartEvent = Event<_>()
    let dealStartEvent = Event<_>()
    let exchangeStartEvent = Event<_>()
    let passEvent = Event<_>()
    let exchangeFinishEvent = Event<_>()
    let trickStartEvent = Event<_>()
    let playEvent = Event<_>()
    let trickFinishEvent = Event<_>()
    let earlyFinalizationEvent = Event<_>()
    let dealFinishEvent = Event<_>()
    let gameFinishEvent = Event<_>()
    let sessionFinishEvent = Event<_>()

    /// Triggers the given event safely.
    let trigger (event : Event<_>) arg =
        match syncOpt with
            | Some sync when sync.InvokeRequired ->
                let del = Action (fun _ -> event.Trigger(arg))
                sync.Invoke(del, Array.empty) |> ignore
            | _ -> event.Trigger(arg)

    /// Plays a trick in the given deal.
    let playTrick deal gameScore =
        assert(deal.ClosedDeal.CurrentTrickOpt.Value.Cards.IsEmpty)

            // trick start
        let leader = deal |> OpenDeal.currentPlayer
        trigger trickStartEvent leader

            // each player plays a card
        let deal =
            (deal, leader |> Seat.cycle)
                ||> Seq.fold (fun deal seat ->
                    assert (deal |> OpenDeal.currentPlayer = seat)
                    let card =
                        playerMap.[seat].MakePlay deal gameScore
                    let deal = deal |> OpenDeal.addPlay card
                    trigger playEvent (seat, card, deal)
                    deal)

            // trick finish
        trigger trickFinishEvent ()
        deal

    /// Plays tricks in the given deal.
    let playTricks deal gameScore =

        let rec loop deal curGameScore =

                // play a trick and update the game score (using the original value)
            let deal = playTrick deal curGameScore
            let curGameScore = gameScore + deal.ClosedDeal.Score

                // deal can be finalized?
            match deal |> OpenDeal.tryFinalize with
                | None -> loop deal curGameScore   // no  - play another trick
                | Some score ->                    // yes - all done

                        // update score again with remaining points
                    let curGameScore = curGameScore + score
                    assert
                        (deal.ClosedDeal.Score + score |> Score.sum
                            = OpenDeal.numPointsPerDeal)

                    if deal.ClosedDeal |> ClosedDeal.isComplete |> not then
                        trigger earlyFinalizationEvent score
                    deal, curGameScore

        loop deal gameScore

    /// Plays the given deal.
    let playDeal deal gameScore =
        assert(deal.ClosedDeal |> ClosedDeal.numCardsPlayed = 0)

            // exchange?
        let deal =
            if deal.Exchange |> Exchange.isHold then deal
            else
                    // exchange start
                let leader = deal |> OpenDeal.currentPlayer
                trigger exchangeStartEvent leader

                    // each player passes cards
                let deal =
                    (deal, leader |> Seat.cycle)
                        ||> Seq.fold (fun deal seat ->
                            assert (deal |> OpenDeal.currentPlayer = seat)
                            let cards =
                                playerMap.[seat].MakePass deal gameScore
                            let deal = deal |> OpenDeal.addPass cards
                            trigger passEvent (seat, cards, deal)
                            deal)

                    // exchange finish
                trigger exchangeFinishEvent ()
                deal

            // playout
        let deal, gameScore =
            assert(deal.ClosedDeal.Score = Score.zero)
            let deal = deal |> OpenDeal.startPlay
            playTricks deal gameScore

        // to-do: shoot the moon
        deal, gameScore

    /// Number of points that ends the game.
    let gameOverThreshold = 100

    /// Determines seats that won the given game score, if any.
    let winningSeats gameScore =

            // is game over?
        let seatPoints =
            let (ScoreMap scoreMap) = gameScore
            scoreMap |> Map.toSeq
        let points =
            seatPoints |> Seq.map snd
        let maxPoints =
            points |> Seq.max
        if maxPoints >= gameOverThreshold then

                // find winning seats
            let minPoints =
                points |> Seq.max
            seatPoints
                |> Seq.where (fun (_, points) ->
                    points = minPoints)
                |> Seq.map fst
                |> set
        else
            Set.empty

    /// Plays a game.
    let playGame dealer dir createDeal =

        /// Plays deals with rotating dealer and exchange direction.
        let rec loop (dealer : Seat) dir gameScore =

                // play one deal
            trigger dealStartEvent (dir, gameScore)
            let deal = createDeal dealer dir
            let _, gameScore = playDeal deal gameScore
            trigger dealFinishEvent (deal, gameScore)

                // continue this game?
            let dealer = dealer.Next
            let dir = ExchangeDirection.next dir
            if gameScore |> winningSeats |> Set.isEmpty then
                loop dealer dir gameScore
            else
                dealer, dir, gameScore

        trigger gameStartEvent dealer
        let dealer, dir, gameScore = loop dealer dir Score.zero
        trigger gameFinishEvent gameScore
        dealer, dir, gameScore

    member __.Run(dealer, dir, createDeal) =
        trigger sessionStartEvent ()
        ((dealer, dir), Seq.init 100 id)
            ||> Seq.fold (fun (dealer, dir) _ ->
                let _, dir, _ = playGame dealer dir createDeal
                dealer.Next, dir)   // KH convention
            |> ignore
        trigger sessionFinishEvent ()

    /// A session has started.
    [<CLIEvent>]
    member __.SessionStartEvent = sessionStartEvent.Publish

    /// A game has started.
    [<CLIEvent>]
    member __.GameStartEvent = gameStartEvent.Publish

    /// A deal has started.
    [<CLIEvent>]
    member __.DealStartEvent = dealStartEvent.Publish

    /// An auction has started.
    [<CLIEvent>]
    member __.ExchangeStartEvent = exchangeStartEvent.Publish

    /// A pass has been made.
    [<CLIEvent>]
    member __.PassEvent = passEvent.Publish

    /// An exchange has finished.
    [<CLIEvent>]
    member __.ExchangeFinishEvent = exchangeFinishEvent.Publish

    /// A trick has started.
    [<CLIEvent>]
    member __.TrickStartEvent = trickStartEvent.Publish

    /// A card has been played.
    [<CLIEvent>]
    member __.PlayEvent = playEvent.Publish

    /// A trick has finished.
    [<CLIEvent>]
    member __.TrickFinishEvent = trickFinishEvent.Publish

    /// A deal has finalized early.
    [<CLIEvent>]
    member __.EarlyFinalizationEvent = earlyFinalizationEvent.Publish

    /// A deal has finished.
    [<CLIEvent>]
    member __.DealFinishEvent = dealFinishEvent.Publish

    /// A game has finished.
    [<CLIEvent>]
    member __.GameFinishEvent = gameFinishEvent.Publish

    /// A session has finished.
    [<CLIEvent>]
    member __.SessionFinishEvent = sessionFinishEvent.Publish
