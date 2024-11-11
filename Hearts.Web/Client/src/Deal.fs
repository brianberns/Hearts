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

    /// Elements tracking current deal score.
    let private dealScoreMap =
        Map [
            Seat.West,  ~~"#wDealScore"
#if !MINI
            Seat.North, ~~"#nDealScore"
#endif
            Seat.East,  ~~"#eDealScore"
            Seat.South, ~~"#sDealScore"
        ]

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

    /// Displays score for each player.
    let private displayScore persState =
        for seat in Enum.getValues<Seat> do
            gameScoreMap[seat].text($"{persState.GameScore[seat]}")
            gamesWonMap[seat].text($"{persState.GamesWon[seat]}")

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

    /// End of game point threshold.
    let private gameThreshold = 100

    /// Finds game winners, if any, in the given score.
    let private findGameWinners score =
        let isOver =
            score.ScoreMap.Values
                |> Seq.exists (fun points ->
                    points >= gameThreshold)
        if isOver then
            let minPoints = Seq.min score.ScoreMap.Values
            score.ScoreMap
                |> Map.toSeq
                |> Seq.where (snd >> (=) minPoints)
                |> Seq.map fst
                |> set
        else Set.empty

    /// Runs one deal.
    let run surface persState =
        async {

                // new deal needed?
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
            displayScore persState
            DealView.displayStatus deal
            let! seatViews =
                DealView.start surface dealer deal
                    |> Async.AwaitPromise

                // run the playout
            let! persState = playout surface persState seatViews

                // deal is over
            match OpenDeal.tryFinalScore persState.Deal with
                | Some score ->

                        // persist updated state
                    let gameScore, gamesWon =
                        let gameScore = persState.GameScore + score
                        let winners = findGameWinners gameScore
                        let gameScore =
                            if winners.IsEmpty then gameScore
                            else Score.zero
                        let gamesWon =
                            (persState.GamesWon, winners)
                                ||> Seq.fold (fun score seat ->
                                    score + Score.create seat 1)
                        gameScore, gamesWon
                    let persState' =
                        { persState with
                            GameScore = gameScore
                            GamesWon = gamesWon
                            Dealer = persState.Dealer.Next
                            DealOpt = None }.Save()

                        // display deal results
                    let shooterOpt =
                        OpenDeal.tryFindShooter
                            persState.Deal.ClosedDeal.Score
                    do! dealOver surface shooterOpt
                        |> Async.AwaitPromise
                    displayScore persState'

                    return persState'

                | None ->
                    return failwith "Incomplete deal"
        }
