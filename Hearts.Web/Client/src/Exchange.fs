namespace Hearts.Web.Client

open Browser.Dom

open Fable.Core

open PlayingCards
open Hearts

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
    let private logHint
        (infoSet : InformationSet) (legalActions : _[]) =
        assert(InformationSet.legalActions infoSet |> snd
            = legalActions)
        async {
            let! strategy = Remoting.getStrategy infoSet
            let pairs =
                Array.zip legalActions strategy
                    |> Seq.sortByDescending snd
            console.log("Pass hint:")
            for (card : Card), prob in pairs do
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

                // exchange is complete?
            // let exchangeComplete = Exchange.isComplete deal.ExchangeOpt

            return deal
        }

    /// Allows user to pass a card.
    let private passUser chooser (handView : HandView) context =

            // determine all legal passes
        let infoSet = OpenDeal.currentInfoSet context.Deal
        let _, legalActions =
            InformationSet.legalActions infoSet
        assert(legalActions.Length > 0)

            // enable user to select card views
        Promise.create(fun resolve _reject ->

                // prompt user to pass
            chooser |> PassChooser.display
            logHint infoSet legalActions

                // handle card clicks
            for cardView in handView do
                cardView.addClass("active")
                cardView.click(fun () ->
                    cardView.addClass("pass")

                    (*
                        // pass the selected card
                    promise {
                        let! deal = passCard context cardView card
                        resolve deal
                    } |> ignore
                    *)
                    promise {
                        do! context.AnimCardPass cardView
                            |> Animation.run
                    } |> ignore))

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

    /// Runs the given deal's exchange.
    let run (persState : PersistentState) chooser (exchangeMap : Map<_, _>) =

        let dealer = persState.Dealer

        /// Passes a single card and then loops recursively.
        let rec loop (persState : PersistentState) =
            async {
                let deal = persState.Deal
                let isComplete =
                    OpenDeal.getExchange deal
                        |> Exchange.isComplete 
                if isComplete then
                    return persState
                else
                        // prepare current passer
                    let seat = OpenDeal.currentPlayer deal
                    let (handView : HandView),
                        animCardPass =
                            exchangeMap[seat]
                    let passer =
                        if seat.IsUser then
                            passUser chooser handView >> Async.AwaitPromise
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
