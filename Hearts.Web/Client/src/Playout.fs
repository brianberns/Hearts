namespace Hearts.Web.Client

open Browser.Dom

open Fable.Core

open PlayingCards
open Hearts

module Playout =

    /// Playout context.
    type private Context =
        {
            /// Current dealer's seat.
            Dealer : Seat

            /// Current deal.
            Deal : OpenDeal

            /// Animation of playing a card.
            AnimCardPlay : CardView -> Animation

            /// Animation of winning a trick.
            AnimTrickFinish : Seat -> Animation
        }

    /// Logs hint information.
    let private logHint
        (infoSet : InformationSet) deal (legalActions : _[]) =
        assert(infoSet.Deal = deal.ClosedDeal)
        assert(InformationSet.legalActions infoSet |> snd
            = legalActions)
        async {
            match deal |> OpenDeal.tryFindInevitable with

                | Some score ->
                    console.log("Inevitable:")
                    for seat, points in Map.toSeq score.ScoreMap do
                        console.log($"   {Seat.toString seat}: {points} point(s)")

                | None when legalActions.Length > 1 ->
                    let! strategy = Remoting.getStrategy infoSet
                    let pairs =
                        Array.zip legalActions strategy
                            |> Seq.sortByDescending snd
                    console.log("Hint:")
                    for (card : Card), prob in pairs do
                        console.log($"   {card}: %.1f{100. * prob}%%")

                | _ -> ()
        } |> Async.StartImmediate

    /// Plays the given card on the current trick, and returns the
    /// seat of the resulting trick winner, if any.
    let private getTrickWinnerOpt context card =
        option {
                // play card on current trick
            assert(context.Deal.ClosedDeal.CurrentTrickOpt.IsSome)
            let! trick = context.Deal.ClosedDeal.CurrentTrickOpt
            let trick' = Trick.addPlay card trick

                // if this card completes the trick, determine winner
            assert(Trick.highPlayerOpt trick' |> Option.isSome)
            let! highPlayer = Trick.highPlayerOpt trick'
            if Trick.isComplete trick' then
                return highPlayer
        }

    /// Plays the given card in the given deal and then continues
    /// the rest of the deal.
    let private playCard context cardView card =
        assert(cardView |> CardView.card = card)
        promise {

                // write to log
            let seat =
                context.Deal.ClosedDeal |> ClosedDeal.currentPlayer
            console.log($"{Seat.toString seat} plays {card}")

                // add the card to the deal
            let deal = OpenDeal.addPlay card context.Deal

                // play the card
            do! context.AnimCardPlay cardView
                |> Animation.run
            DealView.displayStatus deal

                // trick is complete?
            let dealComplete = ClosedDeal.isComplete deal.ClosedDeal
            match getTrickWinnerOpt context card with
                | Some winner ->

                        // animate
                    let animate () =
                        context.AnimTrickFinish winner
                            |> Animation.run
                    if winner.IsUser && not dealComplete then
                        animate () |> ignore   // don't force user to wait for animation to finish
                    else
                        do! animate ()

                | None -> ()

            return deal
        }

    /// Allows user to play a card.
    let private playUser chooser (handView : HandView) context =

            // determine all legal plays
        let infoSet = OpenDeal.currentInfoSet context.Deal
        let _, legalActions =
            InformationSet.legalActions infoSet
        assert(legalActions.Length > 0)

            // enable user to select one of the corresponding card views
        Promise.create(fun resolve _reject ->

                // prompt user to play
            chooser |> PlayChooser.display
            logHint infoSet context.Deal legalActions

                // handle card clicks
            let legalPlaySet = set legalActions
            for cardView in handView do
                let card = cardView |> CardView.card
                if legalPlaySet.Contains(card) then
                    cardView.addClass("active")
                    cardView.click(fun () ->

                            // prevent further clicks
                        chooser |> PlayChooser.hide
                        for cardView in handView do
                            cardView.removeClass("active")
                            cardView.removeClass("inactive")
                            cardView.off("click")

                            // play the selected card
                        promise {
                            let! deal = playCard context cardView card
                            resolve deal
                        } |> ignore)
                else
                    cardView.addClass("inactive"))

    /// Automatically plays a card.
    let private playAuto context =
        async {
                // determine card to play
            let! card = WebPlayer.makePlay context.Deal

                // create view of the selected card
            let! cardView =
                card
                    |> CardView.ofCard
                    |> Async.AwaitPromise

                // play the card
            return! playCard context cardView card
                |> Async.AwaitPromise
        }

    /// Runs the given deal's playout
    let run (persState : PersistentState) chooser (playoutMap : Map<_, _>) =

        let dealer = persState.Dealer

        /// Plays a single card and then loops recursively.
        let rec loop (persState : PersistentState) =
            async {
                let deal = persState.Deal
                let isComplete =
                    ClosedDeal.isComplete deal.ClosedDeal
                if isComplete then
                    return persState
                else
                        // prepare current player
                    let seat = ClosedDeal.currentPlayer deal.ClosedDeal
                    let (handView : HandView),
                        animCardPlay,
                        animTrickFinish =
                            playoutMap[seat]
                    let player =
                        if seat.IsUser then
                            playUser chooser handView >> Async.AwaitPromise
                        else
                            playAuto

                        // invoke player
                    let! deal' =
                        player {
                            Dealer = dealer
                            Deal = deal
                            AnimCardPlay = animCardPlay
                            AnimTrickFinish = animTrickFinish
                        }

                        // recurse until playout is complete
                    let persState' =
                        { persState with DealOpt = Some deal' }
                    let save =   // save at trick boundary
                        match deal'.ClosedDeal.CurrentTrickOpt with   // to-do: clean this up
                            | Some trick ->
                                trick.Cards.Length % Seat.numSeats = 0
                            | None -> true
                    if save then
                        PersistentState.save persState'
                    return! loop persState'
            }

        loop persState
