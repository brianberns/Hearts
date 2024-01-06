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

    /// Handles the end of a deal.
    let private dealOver (surface : JQueryElement) dealer deal =

            // display banner
        let banner =
            let html =
                $"<p>Deal over</p>"
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
            // DealView.displayStatus dealer deal
            let! seatViews =
                DealView.start surface dealer deal
                    |> Async.AwaitPromise

                // run the playout
            let! persState = playout surface persState seatViews
            do! dealOver surface dealer persState.Deal
                |> Async.AwaitPromise
            return persState
        }
