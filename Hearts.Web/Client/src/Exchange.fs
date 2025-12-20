namespace Hearts.Web.Client

open Browser.Dom
open Fable.Core

open PlayingCards
open Hearts

/// Widget that prompts the user to choose a legal pass.
module PassChooser =

    /// Chooser element.
    let element = ~~"#passChooser"

    /// Sets the chooser's exchange direction.
    let setExchangeDirection dir =
        let sDir =
            (ExchangeDirection.toString dir)
                .ToLower()
        (~~"#exchangeDir").text(sDir)

/// Widget that prompts the user to accept a received pass.
module AcceptChooser =

    /// Chooser element.
    let element = ~~"#acceptChooser"

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
    let private passUser (handView : HandView) context =

            // prompt user to pass
        PassChooser.element.show()
        logHint context.Deal

            // handle card clicks
        let cardViews =
            System.Collections.Generic.HashSet<CardView>(
                (*Pass.numCards*))   // Fable doesn't support capacity argument
        for iCard, cardView in Seq.indexed handView do
            cardView.addClass("active")
            cardView.click(fun () ->
                lock cardViews (fun () ->

                        // animate (de-)selected card
                    let anim =
                        if cardViews.Remove(cardView) then
                            OpenHandView.passDeselectAnim
                        else
                            let flag = cardViews.Add(cardView)
                            assert(flag)
                            OpenHandView.passSelectAnim
                    anim cardView handView.Count iCard
                        |> Animation.run
                        |> ignore

                        // complete pass is ready?
                    let toggleClass =
                        let nRemaining = Pass.numCards - cardViews.Count
                        if nRemaining = 0 then
                            PassChooser.element.addClass
                        else
                            if nRemaining > 0 then
                                (context.Deal, cardViews)
                                    ||> Seq.fold (fun deal cardView ->
                                        let card = CardView.card cardView
                                        OpenDeal.addPass card deal)
                                    |> logHint
                            PassChooser.element.removeClass
                    toggleClass("ready")))

            // handle chooser click
        Promise.create(fun resolve _reject ->
            PassChooser.element.click(fun () ->
                if cardViews.Count = Pass.numCards then

                        // reset UI state
                    PassChooser.element.off("click")
                    PassChooser.element.hide()
                    PassChooser.element.removeClass("ready")
                    for cardView in handView do
                        cardView.off("click")

                        // pass the selected cards
                    promise {
                        let! deal =
                            cardViews
                                |> Seq.toArray
                                |> passCards context
                        resolve deal
                    } |> ignore))
            |> Async.AwaitPromise

    /// Automatically passes cards.
    let private passAuto context =

        /// Passes N cards asynchronously.
        let rec loop n deal cards =
            async {
                if n <= 0 then
                    return cards
                else
                    let! card = WebPlayer.takeAction deal
                    let deal = OpenDeal.addPass card deal
                    let cards = Set.add card cards
                    return! loop (n - 1) deal cards
            }            

        async {
                // determine cards to pass
            let! cards = loop Pass.numCards context.Deal Set.empty

                // create views of the selected cards
            let! cardViews =
                cards
                    |> Seq.map CardView.ofCard
                    |> Promise.all
                    |> Async.AwaitPromise

                // pass the cards
            return! passCards context cardViews
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

    /// Receives the exchange in the given deal.
    let private receive deal (exchangeMap : Map<_, _>) =
        async {

                // create new card views
            let exchange = OpenDeal.getExchange deal
            let! cardViewMap =
                createCardViews
                    deal.ClosedDeal.ExchangeDirection
                    exchange

                // animate receive for each seat
            let anim =
                Animation.Parallel [|
                    for (KeyValue(seat, (_, _, animReceivePass, _)))
                        in exchangeMap do
                        let cardViews = cardViewMap[seat]
                        animReceivePass cardViews : Animation
                |]
            do! anim
                |> Animation.run
                |> Async.AwaitPromise

            return cardViewMap
        }

    /// Accepts the exchange in the given deal.
    let private accept (cardViewMap : Map<_, _>) (exchangeMap : Map<_, _>) =

        AcceptChooser.element.show()

        Promise.create (fun resolve _reject ->
            AcceptChooser.element.click(fun () ->

                    // reset UI state
                AcceptChooser.element.off("click")
                AcceptChooser.element.hide()

                    // animate accept for each seat
                Animation.Parallel [|
                    for (KeyValue(seat : Seat, (_, _, _, animAcceptPass)))
                        in exchangeMap do
                        let cardViews : CardView[] = cardViewMap[seat]
                        animAcceptPass cardViews : Animation
                |]
                    |> Animation.run
                    |> ignore
                    
                resolve ()))

            |> Async.AwaitPromise

    /// Finishes the exchange in the given deal.
    let private finish persState deal exchangeMap =
        async {
                // receive pass
            let! cardViewMap = receive deal exchangeMap

                // wait for user to accept received pass
            do! accept cardViewMap exchangeMap

                // wait a beat before starting play
            do! Animation.Sleep 1500
                |> Animation.run
                |> Async.AwaitPromise

                // start play
            let persState =
                { persState with DealOpt = Some deal }
            persState.Save()
            return persState
        }

    /// Runs the given deal's exchange.
    let run (persState : PersistentState) exchangeMap =

        let dealer = persState.Dealer

        /// Passes a group of cards and then loops recursively.
        let rec loop (persState : PersistentState) =
            async {
                let deal = persState.Deal
                let isComplete =
                    OpenDeal.getExchange deal
                        |> Exchange.isComplete 
                if isComplete then
                    return! finish persState deal exchangeMap
                else
                        // prepare current passer
                    let seat = OpenDeal.currentPlayer deal
                    let (handView : HandView),
                        animCardsPass, _, _ =
                            exchangeMap[seat]
                    let passer =
                        if seat.IsUser then
                            passUser handView
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
