namespace Hearts.Web.Client

open Browser

open Fable.Core

open PlayingCards
open Hearts
open Hearts.Web.Client   // ugly - force AutoOpen

module Deal =

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
#if !MINI
            Seat.North, ~~"#nGameScore"
#endif
            Seat.East,  ~~"#eGameScore"
            Seat.South, ~~"#sGameScore"
        ]

    /// Elements tracking number of games won.
    let private gamesWonMap =
        Map [
            Seat.West,  ~~"#wGamesWon"
#if !MINI
            Seat.North, ~~"#nGamesWon"
#endif
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
                                |> OpenDeal.fromDeck dealer ExchangeDirection.Hold
                                |> OpenDeal.startPlay
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

                // run the playout
            let! persState = playout surface persState seatViews

                // deal is over
            match Game.tryFinalScore persState.Deal persState.GameScore with
                | Some dealScore ->

                        // display deal results
                    let shooterOpt =
                        OpenDeal.tryFindShooter
                            persState.Deal.ClosedDeal.Score
                    do! dealOver surface shooterOpt
                        |> Async.AwaitPromise

                        // update game score
                    let gameScore = persState.GameScore + dealScore
                    displayGameScore gameScore

                        // is the game over?
                    let winners = Game.findGameWinners gameScore
                    let persState' =
                        { persState with
                            GameScore = gameScore
                            Dealer = persState.Dealer.Next
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
