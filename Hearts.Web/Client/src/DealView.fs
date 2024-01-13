namespace Hearts.Web.Client

open PlayingCards
open Hearts

[<AutoOpen>]
module SeatExt =
    type Seat with

        /// The user's seat.
        static member User = Seat.South

        /// Indicates whether the given seat is played by the user.
        member seat.IsUser = (seat = Seat.User)

module Seat =

    /// Answers the index of the given seat relative to the given
    /// base seat.
    let getIndex (seat : Seat) (baseSeat : Seat) =
        let idx = ((int seat) - (int baseSeat) + Seat.numSeats) % Seat.numSeats
        assert(idx >= 0)
        assert(idx < Seat.numSeats)
        idx

module DealView =

    /// Creates card backs at the center of the given surface.
    let private getCardBacks (surface : JQueryElement) =
        let pos = Position.ofInts (50, 50)
        Seq.init Card.numCards (fun _ ->
            promise {
                let! cardView = CardView.ofBack ()
                JQueryElement.setPosition pos cardView
                surface.append(cardView)
                return cardView
            })
            |> Seq.rev
            |> Seq.toArray
            |> Promise.all

    /// Creates a closed hand view of the given card backs.
    let private closedView offset (backs : _[]) =
        let numPlayers = 4
        Seq.init ClosedDeal.numCardsPerHand (fun i ->
            backs[numPlayers * i + offset])
            |> ClosedHandView.ofCardViews

    /// Creates an open hand view of the user's cards.
    let private openView deal =
        promise {
            return! deal.UnplayedCardMap[Seat.User]
                |> OpenHandView.ofHand
        }

    /// Animates the start of the given deal on the given surface.
    let private animate surface dealer deal =
        promise {

                // create closed hand views for dealing
            let! backs = getCardBacks surface
            let closed1 = closedView 0 backs
            let closed2 = closedView 1 backs
            let closed3 = closedView 2 backs
            let closed0 = closedView 3 backs   // dealer receives cards last
            let closedHandViews =
                [| closed0; closed1; closed2; closed3 |]

                // create open hand view for user
            let iUser = Seat.getIndex Seat.User dealer
            let! openHandView = openView deal

                // deal animation
            let anim =

                    // animate hands being dealt
                let seat iPlayer = Seat.incr iPlayer dealer
                let anim1 = HandView.dealAnim (seat 1) closed1
                let anim2 = HandView.dealAnim (seat 2) closed2
                let anim3 = HandView.dealAnim (seat 3) closed3
                let anim0 = HandView.dealAnim (seat 0) closed0

                    // animate user's hand reveal
                let animReveal =
                    let closedHandView = closedHandViews[iUser]
                    OpenHandView.revealAnim closedHandView openHandView

                    // animate remaining deck removal
                let animRemove =
                    backs[24..]
                        |> Array.map (fun back ->
                            Animation.create back Remove)
                        |> Animation.Parallel

                    // assemble complete animation
                [|
                    anim1; anim2; anim3; anim0
                    animReveal; animRemove
                |] |> Animation.Serial 

                // run the deal start animation
            do! Animation.run anim

                // answer the hand views for futher animation
            return closedHandViews
                |> Array.mapi (fun iPlayer closedHandView ->
                    let handView =
                        if iPlayer = iUser then openHandView
                        else closedHandView
                    let seat = Seat.incr iPlayer dealer
                    seat, handView)
        }

    /// Creates and positions hand views for the given deal.
    let private createHandViews (surface : JQueryElement) deal =

        /// Sets positions of cards in the given hand.
        let setPositions seat handView =
            HandView.setPositions seat handView
            for cardView in handView do
                surface.append(cardView)

        /// Creates and positions a closed hand view for the given seat.
        let closedViewPair seat =
            promise {
                let! handView =
                    Array.init
                        deal.UnplayedCardMap[seat].Count
                        (fun _ -> CardView.ofBack ())
                        |> Promise.all
                        |> Promise.map ResizeArray
                setPositions seat handView
                return seat, (handView : HandView)
            }

        promise {

                // create closed hand views
            let! closedHandViewPairs =
                [ Seat.West; Seat.North; Seat.East ]
                    |> Seq.map closedViewPair
                    |> Promise.all

                // create open hand view for user
            let! openHandView = openView deal
            setPositions Seat.South openHandView

            return [|
                yield! closedHandViewPairs
                yield Seat.South, openHandView
            |]
        }

    /// Creates hand views for the given deal.
    let start surface dealer deal =
        if ClosedDeal.numCardsPlayed deal.ClosedDeal = 0 then
            animate surface dealer deal
        else
            createHandViews surface deal   // no animation

    /// Elements tracking current score.
    let private scoreElemMap =
        Map [
            Seat.West,  ~~"#wCurrent"
            Seat.North, ~~"#nCurrent"
            Seat.East,  ~~"#eCurrent"
            Seat.South, ~~"#sCurrent"
        ]

    /// Elements tracking played cards.
    let private suitElemMap =
        Map [
            Suit.Clubs,    ~~"#deckClubs"
            Suit.Diamonds, ~~"#deckDiamonds"
            Suit.Hearts,   ~~"#deckHearts"
            Suit.Spades,   ~~"#deckSpades"
        ]

    /// Gets the ID of the table cell that represents the
    /// given card.
    let private getCellId (card : Card) =
        $"deck{card.String}"

    /// Prepares deck table for use.
    let prepareDeckTable () =

        let ranksRow = ~~"#deckRanks"
        for rank in Enum.getValues<Rank> do

                // one column for each rank
            let rankChar = Rank.toChar rank
            let cell =
                ~~HTMLTableHeaderCellElement.Create(
                    innerText = $"{rankChar}")
            ranksRow.append(cell)

                // empty cell for each card
            for suit in Enum.getValues<Suit> do
                let row = suitElemMap[suit]
                let cell =
                    let card = Card.create rank suit
                    ~~HTMLTableCellElement.Create(
                        id = getCellId card)
                row.append(cell)

    /// Displays current deal status.
    let displayStatus deal =

            // current score for each player
        for seat in Enum.getValues<Seat> do
            scoreElemMap[seat].text($"{deal.ClosedDeal.Score[seat]}")

            // status of each card
        let deckView = ~~"#deck"
        if deal.ClosedDeal.CurrentTrickOpt.IsSome
            && OpenDeal.currentPlayer deal = Seat.User then
            deckView.show()
            let handCards =
                deal
                    |> OpenDeal.currentHand
                    |> set
            for card in Card.allCards do
                let text =
                    if deal.ClosedDeal.PlayedCards.Contains(card) then "🞬"
                    elif handCards.Contains(card) then "⬤"
                    else ""
                (~~($"#{getCellId card}")).text(text)
        else
            deckView.hide()
