namespace Hearts.Web.Client

open Browser.Dom
open Fable.Core

open PlayingCards
open Hearts

/// Widget that prompts the user to choose a legal pass.
type PassChooser =
    {
        /// Underlying HTML element.
        Element : JQueryElement
    }

module PassChooser =

    /// Creates a chooser.
    let create dir =

            // set exchange direction text
        let sDir =
            (ExchangeDirection.toString dir)
                .ToLower()
        (~~"#exchangeDir").text(sDir)

        { Element = ~~"#passChooser" }

    /// Makes the given chooser visible.
    let display chooser =
        chooser.Element.css {| display = "block" |}

    /// Makes the given chooser invisible.
    let hide chooser =
        chooser.Element.css {| display = "none" |}

module Exchange =

    /// Exchange context.
    type private Context =
        {
            /// Current dealer's seat.
            Dealer : Seat

            /// Current deal.
            Deal : OpenDeal

            /// Animation of passing cards.
            AnimCardsPass : CardView[] -> Animation
        }

    /// Logs hint information.
    let private logHint deal =
        async {
            let infoSet = OpenDeal.currentInfoSet deal
            let! strategy = Remoting.getStrategy infoSet
            let pairs =
                Array.zip (Seq.toArray infoSet.Hand) strategy
                    |> Seq.sortByDescending snd
            console.log("Pass hint:")
            for card, prob in pairs do
                console.log($"   {card}: %.1f{100. * prob}%%")
        } |> Async.StartImmediate

    /// Passes the given cards in the given deal and then continues
    /// the rest of the deal.
    let private passCards context cardViews =
        let cards =
            cardViews
                |> Array.map CardView.card
        promise {

                // write to log
            let seat = OpenDeal.currentPlayer context.Deal
            let str =
                cards
                    |> Seq.map _.String
                    |> String.concat ", "
            console.log($"{Seat.toString seat} passes {str}")

                // add the cards to the deal
            let deal =
                (context.Deal, cards)
                    ||> Seq.fold (fun deal card ->
                        OpenDeal.addPass card deal)

                // pass the cards
            do! context.AnimCardsPass cardViews
                |> Animation.run
            DealView.displayStatus deal

            return deal
        }

    /// Allows user to pass cards.
    let private passUser chooser (handView : HandView) context =

            // prompt user to pass
        chooser |> PassChooser.display
        logHint context.Deal

            // handle card clicks
        let cardViews =
            System.Collections.Generic.HashSet<CardView>(
                (*Pass.numCards*))   // Fable doesn't support capacity argument
        let btn = ~~"#passChooserBtn"
        for cardView in handView do
            cardView.addClass("active")
            cardView.click(fun () ->
                lock cardViews (fun () ->
                    if cardViews.Remove(cardView) then
                        OpenHandView.passDeselectAnim cardView
                            |> Animation.run
                            |> ignore
                    else
                        let flag = cardViews.Add(cardView)
                        assert(flag)
                        OpenHandView.passSelectAnim cardView
                            |> Animation.run
                            |> ignore

                    btn.prop(
                        "disabled",
                        cardViews.Count <> Pass.numCards)))

            // handle "Ready" button click
        Promise.create(fun resolve _reject ->
            btn.click(fun () ->
                chooser |> PassChooser.hide
                promise {
                    let! deal =
                        cardViews
                            |> Seq.toArray
                            |> passCards context
                    resolve deal
                } |> ignore))
            |> Async.AwaitPromise

    /// Automatically passes a card.
    let private passAuto context =
        async {
                // determine card to pass
            let! card = WebPlayer.takeAction context.Deal

                // create view of the selected card
            let! cardView =
                card
                    |> CardView.ofCard
                    |> Async.AwaitPromise

                // pass the card
            return! passCards context [|cardView|]
                |> Async.AwaitPromise
        }

    /// Creates new views for incoming cards in the given exchange.
    let private createCardViews dir exchange =

        let asyncPairs =
            Async.Parallel [|
                for fromSeat in Enum.getValues<Seat> do

                        // get target seat
                    let toSeat =
                        ExchangeDirection.apply fromSeat dir

                        // function to create a new card view
                    let createCardView =
                        if toSeat.IsUser then
                            CardView.ofCard
                        else
                            fun _ -> CardView.ofBack ()

                        // create new card views
                    async {
                        let! cardViews =
                            exchange.PassMap[fromSeat]
                                |> Seq.sortBy OpenHandView.sortKey
                                |> Seq.map (createCardView >> Async.AwaitPromise)
                                |> Async.Parallel
                        return toSeat, cardViews
                    }
            |]

        async {
            let! pairs = asyncPairs
            return Map pairs
        }

    /// Finishes the exchange in the given deal.
    let private finish deal (exchangeMap : Map<_, _>) =
        async {

                // create new card views
            let exchange = OpenDeal.getExchange deal
            let! cardViewMap =
                createCardViews
                    deal.ClosedDeal.ExchangeDirection
                    exchange

                // animate finish for each seat
            let anim =
                Animation.Parallel [|
                    for (KeyValue(seat, (_, _, animReceivePass)))
                        in exchangeMap do
                        let cardViews = cardViewMap[seat]
                        animReceivePass cardViews : Animation
                |]
            do! Animation.run anim |> Async.AwaitPromise

                // start play
            return OpenDeal.startPlay deal
        }

    /// Runs the given deal's exchange.
    let run (persState : PersistentState) chooser exchangeMap =

        let dealer = persState.Dealer

        /// Passes a single card and then loops recursively.
        let rec loop (persState : PersistentState) =
            async {
                let deal = persState.Deal
                let isComplete =
                    OpenDeal.getExchange deal
                        |> Exchange.isComplete 
                if isComplete then
                    let! deal' = finish deal exchangeMap
                    return { persState with DealOpt = Some deal' }
                else
                        // prepare current passer
                    let seat = OpenDeal.currentPlayer deal
                    let (handView : HandView),
                        animCardsPass, _ =
                            exchangeMap[seat]
                    let passer =
                        if seat.IsUser then
                            passUser chooser handView
                        else
                            passAuto

                        // invoke passer
                    let! deal' =
                        passer {
                            Dealer = dealer
                            Deal = deal
                            AnimCardsPass = animCardsPass
                        }

                        // recurse until exchange is complete
                    let persState' =
                        { persState with DealOpt = Some deal' }
                    return! loop persState'
            }

        loop persState
