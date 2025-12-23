namespace Hearts.Web.Client

open Browser.Dom

open Fable.Core

open PlayingCards
open Hearts

/// Widget that prompts the user to choose a legal play.
module PlayChooser =

    /// Chooser element.
    let element = ~~"#playChooser"

module Playout =

    /// Playout context.
    type private Context =
        {
            /// Current dealer's seat.
            Dealer : Seat

            /// Current game state.
            Game : Game

            /// Animation of playing a card.
            AnimCardPlay : CardView -> Animation

            /// Animation of winning a trick.
            AnimTrickFinish : Seat -> Animation
        }

    /// Logs hint information.
    let private logHint game =
        let infoSet = Game.currentInfoSet game
        let legalActions = infoSet.LegalActions
        match game.Deal |> OpenDeal.tryFindInevitable with

            | Some score ->
                console.log("Inevitable:")
                for seat, points in Score.indexed score do
                    console.log($"   {Seat.toString seat}: {points} point(s)")

            | None when legalActions.Length > 1 ->
                async {
                    let! strategy = Remoting.getStrategy infoSet
                    let pairs =
                        Array.zip legalActions strategy
                            |> Seq.sortByDescending snd
                    console.log("Play hint:")
                    for card, prob in pairs do
                        console.log($"   {card}: %.1f{100. * prob}%%")
                } |> Async.StartImmediate

            | _ -> ()

    /// Plays the given card on the current trick, and returns the
    /// seat of the resulting trick winner, if any.
    let private getTrickWinnerOpt context card =
        option {
                // play card on current trick
            assert(context.Game.Deal.ClosedDeal.CurrentTrickOpt.IsSome)
            let! trick = context.Game.Deal.ClosedDeal.CurrentTrickOpt
            let trick' = Trick.addPlay card trick

                // if this card completes the trick, determine winner
            assert(Trick.highPlayerOpt trick' |> Option.isSome)
            let! highPlayer = Trick.highPlayerOpt trick'
            if Trick.isComplete trick' then
                return highPlayer
        }

    /// Plays the given card in the given deal and then continues
    /// the rest of the deal.
    let private playCard context cardView =
        let card = cardView |> CardView.card
        promise {

                // write to log
            let seat =
                OpenDeal.currentPlayer context.Game.Deal
            console.log($"{Seat.toString seat} plays {card}")

                // add the card to the deal
            let deal = OpenDeal.addPlay card context.Game.Deal

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
    let private playUser (handView : HandView) context =

            // determine all legal actions
        let infoSet = Game.currentInfoSet context.Game
        let legalActions = infoSet.LegalActions
        assert(legalActions.Length > 0)

            // enable user to select one of the corresponding card views
        Promise.create(fun resolve _reject ->

                // prompt user to play
            PlayChooser.element.show()
            logHint context.Game

                // handle card clicks
            let legalPlaySet = set legalActions
            for cardView in handView do
                let card = cardView |> CardView.card
                if legalPlaySet.Contains(card) then
                    cardView.addClass("active")
                    cardView.click(fun () ->

                            // prevent further clicks
                        PlayChooser.element.hide()
                        for cardView in handView do
                            cardView.removeClass("active")
                            cardView.removeClass("inactive")
                            cardView.off("click")

                            // play the selected card
                        promise {
                            let! deal = playCard context cardView
                            resolve deal
                        } |> ignore)
                else
                    cardView.addClass("inactive"))

            |> Async.AwaitPromise

    /// Automatically plays a card.
    let private playAuto context =
        async {
                // determine card to play
            let! card = WebPlayer.takeAction context.Game

                // create view of the selected card
            let! cardView =
                card
                    |> CardView.ofCard
                    |> Async.AwaitPromise

                // play the card
            return! playCard context cardView
                |> Async.AwaitPromise
        }

    /// Runs the given deal's playout
    let run (persState : PersistentState) (playoutMap : Map<_, _>) =

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
                            playUser handView
                        else
                            playAuto

                        // invoke player
                    let! deal' =
                        let game = Game.create deal persState.GameScore   // to-do: store game in persState
                        player {
                            Dealer = dealer
                            Game = game
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
