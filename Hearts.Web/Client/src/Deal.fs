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

    /// Elements tracking total score.
    let private elemMap =
        Map [
            Seat.West,  ~~"#wTotal"
#if !MINI
            Seat.North, ~~"#nTotal"
#endif
            Seat.East,  ~~"#eTotal"
            Seat.South, ~~"#sTotal"
        ]

    /// Displays total score for each player.
    let displayScore persState =
        for seat in Enum.getValues<Seat> do
            elemMap[seat].text($"{persState.TotalScore[seat]}")

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
                    let persState' =
                        { persState with
                            TotalScore =
                                persState.TotalScore + score
                            Dealer = persState.Dealer.Next
                            DealOpt = None }.Save()

                        // display deal results
                    let shooterOpt =
                        OpenDeal.tryFindShooter persState.Deal
                    do! dealOver surface shooterOpt
                        |> Async.AwaitPromise
                    displayScore persState'

                    return persState'

                | None ->
                    return failwith "Incomplete deal"
        }
