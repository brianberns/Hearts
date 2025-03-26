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

            // gets element to prompt the user
        let div = ~~"#passChooser"
        do
            let sDir =
                (ExchangeDirection.toString dir)
                    .ToLower()
            assert(Pass.numCards = 3)
            div.text($"Pass three cards {sDir}")

        { Element = div }

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

            /// Animation of passing a card.
            AnimCardPass : CardView -> Animation
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

    /// Passes the given card in the given deal and then continues
    /// the rest of the deal.
    let private passCard context cardView card =
        assert(cardView |> CardView.card = card)
        promise {

                // write to log
            let seat = OpenDeal.currentPlayer context.Deal
            console.log($"{Seat.toString seat} passes {card}")

                // add the card to the deal
            let deal = OpenDeal.addPass card context.Deal

                // pass the card
            do! context.AnimCardPass cardView
                |> Animation.run
            DealView.displayStatus deal

            return deal
        }

    /// Allows user to pass a card.
    let private passUser chooser (handView : HandView) context =
        let passCards =
            System.Collections.Generic.HashSet<CardView>(
                (*Pass.numCards*))
        Promise.create(fun resolve _reject ->

                // prompt user to pass
            chooser |> PassChooser.display
            logHint context.Deal

                // handle card clicks
            for cardView in handView do
                cardView.addClass("active")
                cardView.click(fun () ->
                    promise {
                        if passCards.Remove(cardView) then
                            do! OpenHandView.passDeselectAnim cardView
                                |> Animation.run
                        else
                            let flag = passCards.Add(cardView)
                            assert(flag)
                            do! OpenHandView.passSelectAnim cardView
                                |> Animation.run

                        if passCards.Count = Pass.numCards then
                            chooser |> PassChooser.hide
                            let cards = passCards |> Seq.toArray
                            let! deal = passCard context cards[0] (cards[0] |> CardView.card)
                            let context = { context with Deal = deal }
                            let! deal = passCard context cards[1] (cards[1] |> CardView.card)
                            let context = { context with Deal = deal }
                            let! deal = passCard context cards[2] (cards[2] |> CardView.card)
                            resolve deal
                    } |> ignore))
                |> Async.AwaitPromise

        (*
            // enable user to select one of the corresponding card views
        Promise.create(fun resolve _reject ->

                // prompt user to pass
            chooser |> PassChooser.display
            logHint context.Deal

                // handle card clicks
            for cardView in handView do
                let card = cardView |> CardView.card
                cardView.addClass("active")
                cardView.click(fun () ->

                        // prevent further clicks
                    chooser |> PassChooser.hide
                    for cardView in handView do
                        cardView.removeClass("active")
                        cardView.off("click")

                        // pass the selected card
                    promise {
                        let! deal = passCard context cardView card
                        resolve deal
                    } |> ignore))
        *)

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
            return! passCard context cardView card
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
                        animCardPass, _ =
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
                            AnimCardPass = animCardPass
                        }

                        // recurse until exchange is complete
                    let persState' =
                        { persState with DealOpt = Some deal' }
                    return! loop persState'
            }

        loop persState
