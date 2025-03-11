﻿namespace Hearts.Web.Client

open Browser

open Fable.Core

open PlayingCards
open Hearts
open Hearts.Web.Client   // ugly - force AutoOpen

module Deal =

    /// Runs the exchange of the given deal.
    let private exchange
        dir
        (surface : JQueryElement)
        persState
        handViews =

            // create pass chooser
        let chooser = PassChooser.create dir
        surface.append(chooser.Element)

        let passPosMap =
            Position.seatMap [
                Seat.West,  (20, 69)
                Seat.North, (28, 16)
                Seat.East,  (80, 31)
                Seat.South, (72, 83)
            ]

            // get animations for each seat
        let exchangeMap =
            handViews
                |> Seq.map (fun (seat : Seat, handView : HandView) ->

                    let animCardPass : CardView -> Animation =
                        if seat.IsUser then
                            fun (cardView : CardView) ->
                                let pos =
                                    JQueryElement.getPosition cardView
                                        + Position.ofInts(0, -3)
                                AnimationAction.moveTo pos
                                    |> Animation.create cardView
                        else
                            fun (cardView : CardView) ->

                                    // horizontal offset (-1, 0, 1)
                                let offset =
                                    assert(Pass.numCards = 3)
                                    ClosedDeal.numCardsPerHand
                                        - handView.Count
                                        - 1

                                    // remove arbitrary card from hand
                                let back = Seq.last handView
                                assert(CardView.isBack back)
                                let flag = handView.Remove(back)
                                assert(flag)

                                    // pass in given direction
                                let pos =
                                    let targetSeat =
                                        ExchangeDirection.apply seat dir
                                    passPosMap[targetSeat]
                                        + Position.ofInts(offset, 0)
                                [|
                                    AnimationAction.BringToFront
                                    AnimationAction.moveTo pos
                                |]
                                    |> Array.map (Animation.create back)
                                    |> Animation.Parallel

                    let tuple =
                        handView,
                        animCardPass

                    seat, tuple)
                |> Map

            // run the exchange
        async {
            let! persState' = Exchange.run persState chooser exchangeMap
            chooser.Element.remove()
            return persState'
        }

    /// Runs the playout of the given deal.
    let private playout
        (surface : JQueryElement)
        persState
        handViews =

            // create play chooser
        let chooser = PlayChooser.create ()
        surface.append(chooser.Element)

            // get animations for each seat
        let playoutMap =
            handViews
                |> Seq.map (fun (seat : Seat, handView) ->

                    let animCardPlay =
                        let anim =
                            if seat.IsUser then OpenHandView.playAnim
                            else ClosedHandView.playAnim
                        anim seat handView

                    let tuple =
                        handView,
                        animCardPlay,
                        TrickView.finishAnim

                    seat, tuple)
                |> Map

            // run the playout
        async {
            let! persState' = Playout.run persState chooser playoutMap
            chooser.Element.remove()
            return persState'
        }

    /// Elements tracking current game score.
    let private gameScoreMap =
        Map [
            Seat.West,  ~~"#wGameScore"
            Seat.North, ~~"#nGameScore"
            Seat.East,  ~~"#eGameScore"
            Seat.South, ~~"#sGameScore"
        ]

    /// Elements tracking number of games won.
    let private gamesWonMap =
        Map [
            Seat.West,  ~~"#wGamesWon"
            Seat.North, ~~"#nGamesWon"
            Seat.East,  ~~"#eGamesWon"
            Seat.South, ~~"#sGamesWon"
        ]

    /// Displays game score for each player.
    let private displayGameScore (gameScore : Score) =
        for seat in Enum.getValues<Seat> do
            gameScoreMap[seat].text($"{gameScore[seat]}")

    /// Handles the end of a deal.
    let private dealOver (surface : JQueryElement) shooterOpt =

            // display banner
        let banner =
            let html =
                shooterOpt
                    |> Option.map (fun shooter ->
                        console.log($"{Seat.toString shooter} shot the moon!")
                        $"{Seat.toString shooter} shot the moon!<br /><span style=\"font-size: 100px\">🎆🌕🎆</span>")
                    |> Option.defaultValue "Deal is over"
            ~~HTMLDivElement.Create(innerHTML = html)
        banner.addClass("banner")
        surface.append(banner)

            // wait for user to click banner
        Promise.create (fun resolve _reject ->
            banner.click(fun () ->
                banner.remove()
                resolve ()))

    /// Displays games won for each player.
    let private displayGamesWon (gamesWon : Score) =
        for seat in Enum.getValues<Seat> do
            gamesWonMap[seat].text($"{gamesWon[seat]}")

    /// Increments the number of games won by the given players.
    let private incrGamesWon winners persState =

            // increment counts
        let gamesWon =
            (persState.GamesWon, winners)
                ||> Seq.fold (fun score seat ->
                    score + Score.create seat 1)

            // update persistent state
        { persState with
            GamesWon = gamesWon
            GameScore = Score.zero }

    /// Handles the end of a game.
    let private gameOver (surface : JQueryElement) winners gamesWon =

            // display banner
        let banner =
            let sWinners =
                winners
                    |> Seq.map Seat.toString
                    |> String.concat " and "
            let suffix =
                if sWinners.Contains(' ') then ""
                else "s"
            let text = $"{sWinners} win{suffix} the game!"
            console.log($"{sWinners} win{suffix} the game!")
            ~~HTMLDivElement.Create(innerText = text)
        banner.addClass("banner")
        surface.append(banner)

            // wait for user to click banner
        Promise.create (fun resolve _reject ->
            banner.click(fun () ->
                banner.remove()
                displayGamesWon gamesWon
                resolve ()))

    /// Runs one deal.
    let run surface persState =
        async {

                // new deal needed?
            displayGamesWon persState.GamesWon
            let dealer = persState.Dealer
            let deal, persState =
                match persState.DealOpt with

                        // use existing deal
                    | Some deal -> deal, persState

                        // create random deal
                    | None ->
                        let rng = Random(persState.RandomState)
                        do
                            console.log($"Deal #{rng.State}")
                            console.log($"Dealer is {Seat.toString dealer}")
                        let deal =
                            Deck.shuffle rng
                                |> OpenDeal.fromDeck
                                    dealer persState.ExchangeDirection
                        let deal =
                            if deal.ClosedDeal.ExchangeDirection   // can start play immediately?
                                = ExchangeDirection.Hold then
                                OpenDeal.startPlay deal
                            else deal
                        let persState =
                            { persState with
                                RandomState = rng.State
                                DealOpt = Some deal }.Save()
                        deal, persState

                // animate dealing the cards
            displayGameScore persState.GameScore
            DealView.displayStatus deal
            let! seatViews =
                DealView.start surface dealer deal
                    |> Async.AwaitPromise

                // run the exchange?
            let! persState =
                let dir = deal.ClosedDeal.ExchangeDirection
                if dir <> ExchangeDirection.Hold then
                    exchange dir surface persState seatViews
                else
                    async.Return(persState)

                // run the playout
            let! persState = playout surface persState seatViews

                // deal is over
            match Game.tryUpdateScore persState.Deal persState.GameScore with
                | Some gameScore ->

                        // display deal results
                    for seat in Enum.getValues<Seat> do
                        console.log(
                            $"{Seat.toString seat} takes {persState.Deal.ClosedDeal.Score[seat]} point(s)")
                    let shooterOpt =
                        Game.tryFindShooter
                            persState.Deal.ClosedDeal.Score
                    do! dealOver surface shooterOpt
                        |> Async.AwaitPromise

                        // update game score
                    for seat in Enum.getValues<Seat> do
                        console.log(
                            $"{Seat.toString seat} has {gameScore[seat]} point(s)")
                    displayGameScore gameScore

                        // is the game over?
                    let winners = Game.findGameWinners gameScore
                    let persState' =
                        { persState with
                            GameScore = gameScore
                            Dealer = Seat.next persState.Dealer
                            DealOpt = None }
                    if winners.IsEmpty then
                        PersistentState.save persState'
                        return persState'
                    else
                            // increment games won
                        let persState'' =
                            incrGamesWon winners persState'
                        PersistentState.save persState''

                            // display game result
                        do! gameOver surface winners persState''.GamesWon
                            |> Async.AwaitPromise

                        return persState''

                | None ->
                    return failwith "Incomplete deal"
        }
