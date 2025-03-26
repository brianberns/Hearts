namespace Hearts.Web.Client

open PlayingCards
open Hearts

/// Represents a (mutable) hand of cards.
type HandView = ResizeArray<CardView>

module HandView =

    /// Target distance between adjacent cards in the hand.
    let delta = 1.8

    /// Gets the target left coord of the given card in a hand
    /// containing the given total number of cards.
    let private getLeft numCards iCard =

        /// Left-shift from center of hand.
        let shift = 0.5 * float (numCards - 1) * delta

        (delta * float iCard) - shift
            |> Percent

    /// Gets the position of a card in a hand.
    let private getPosition centerPos numCards iCard =
        { centerPos with
            left = getLeft numCards iCard + centerPos.left }

    /// Animates a card to its target position in a hand.
    let private animateCard centerPos numCards iCard =
        getPosition centerPos numCards iCard
            |> AnimationAction.moveTo

    /// Center position of each hand.
    let private centerPosMap =
        Position.seatMap [
            Seat.West,  (20, 50)
            Seat.North, (50, 15)
            Seat.East,  (80, 50)
            Seat.South, (50, 84)
        ]

    /// Deals the cards in the given hand view into their target
    /// position.
    let dealAnim seat (handView : HandView) =
        [|
            for iCard = 0 to handView.Count - 1 do
                let cardView = handView[iCard]
                let actions =
                    let centerPos = centerPosMap[seat]
                    seq {
                        animateCard centerPos handView.Count iCard
                        BringToFront
                    }
                for action in actions do
                    yield Animation.create cardView action
        |] |> Animation.Parallel

    /// Sets the positions of the cards in the given hand, without
    /// animation.
    let setPositions seat (handView : HandView) =
        for iCard = 0 to handView.Count - 1 do
            let cardView = handView[iCard]
            let pos =
                let centerPos = centerPosMap[seat]
                getPosition centerPos handView.Count iCard
            JQueryElement.setPosition pos cardView

    /// Animates adjustment of remaining unplayed cards in a hand.
    let adjustAnim seat (handView : HandView) =
        let numCards = handView.Count
        handView
            |> Seq.mapi (fun iCard cardView ->
                let centerPos = centerPosMap[seat]
                animateCard centerPos numCards iCard
                    |> Animation.create cardView)
            |> Seq.toArray
            |> Animation.Parallel

module ClosedHandView =

    /// Creates a closed view of the given cards.
    let ofCardViews (cardViews : seq<CardView>) : HandView =
        assert(cardViews |> Seq.forall CardView.isBack)
        ResizeArray(cardViews)

    /// Animates the passing of a card from a closed hand view.
    let passAnim seat dir (handView : HandView) _cardViews =
        assert(_cardViews |> Array.length = 1)

            // remove arbitrary card from hand instead
        let cardView = handView |> Seq.last
        assert(CardView.isBack cardView)
        let flag = handView.Remove(cardView)
        assert(flag)

            // animate pass
        Animation.Parallel [|

                // bring card to front
            AnimationAction.BringToFront
                |> Animation.create cardView

                // slide card to opponent's receiving position
            ExchangeView.passAnim
                seat dir cardView HandView.delta
        |]

    /// Animates receiving cards into a closed hand view.
    let receivePassAnim seat dir (handView : HandView) cardViews =

            // get incoming old card views (might be backs or fronts)
        let oldCardViews =
            let fromSeat = ExchangeDirection.unapply seat dir
            ExchangeView.finish fromSeat

            // add incoming new card views to hand view (all backs)
        assert(cardViews |> Seq.forall CardView.isBack)
        handView.AddRange(cardViews)

        Animation.Parallel [|

                // replace old views with new
            yield! Array.map2 (fun oldView newView ->
                Animation.create oldView (ReplaceWith newView))
                oldCardViews
                cardViews

                // move new views into the hand
            yield HandView.dealAnim seat handView
        |]

    /// Animates the playing of a card from a closed hand view.
    let playAnim seat (handView : HandView) (cardView : CardView) =

            // remove arbitrary card from hand
        let back = handView |> Seq.last
        assert(back |> CardView.isBack)
        let flag = handView.Remove(back)
        assert(flag)

            // animate card being played
        Animation.Serial [|

                // bring card to front
            BringToFront
                |> Animation.create back

                // reveal card
            ReplaceWith cardView
                |> Animation.create back

                // slide revealed card to center
            TrickView.playAnim seat cardView
        |]

module OpenHandView =

    /// Sort key for the given card.
    let sortKey card =
        let suitKey =
            match card.Suit with
                | Suit.Spades   -> 1   // black
                | Suit.Hearts   -> 2   // red
                | Suit.Clubs    -> 3   // black
                | Suit.Diamonds -> 4   // red
                | _ -> failwith "Unexpected"
        let rankKey = -1 * int card.Rank
        suitKey, rankKey

    /// Creates an open view of the given hand.
    let ofHand (hand : Hand) : Fable.Core.JS.Promise<HandView> =
        hand
            |> Seq.sortBy sortKey
            |> Seq.map CardView.ofCard
            |> Promise.all
            |> Promise.map ResizeArray

    /// Reveals the given open hand view.
    let revealAnim (closedHandView : HandView) (openHandView : HandView) =
        assert(closedHandView.Count = openHandView.Count)
        (closedHandView, openHandView)
            ||> Seq.map2 (fun back front ->
                Animation.create back (ReplaceWith front))
            |> Seq.toArray
            |> Animation.Parallel

    /// Vertical displacement of a card selected to be passed.
    let private selectOffset = 2

    /// Animates the selection of a card to pass from an open hand view.
    let passSelectAnim (cardView : CardView) =
        let pos = JQueryElement.getPosition cardView
        AnimationAction.moveTo (pos + Position.ofInts(0, -selectOffset))
            |> Animation.create cardView

    /// Animates the deselection of a card to pass from an open hand view.
    let passDeselectAnim (cardView : CardView) =
        let pos = JQueryElement.getPosition cardView
        AnimationAction.moveTo (pos + Position.ofInts(0, selectOffset))
            |> Animation.create cardView

    /// Animates the passing of a card from an open hand view.
    let passAnim seat dir (handView : HandView) cardViews =

            // remove selected cards from hand
        for cardView in cardViews do
            let flag = handView.Remove(cardView)
            assert(flag)

            // animate cards being passed
        [|
            for cardView in cardViews do
                AnimationAction.BringToFront        // bring card to front
                    |> Animation.create cardView
                ExchangeView.passAnim               // slide card to opponent
                    seat dir cardView HandView.delta
            HandView.adjustAnim seat handView       // adjust remaining cards to fill gap
        |] |> Animation.Parallel

    /// Animates receiving cards into an open hand view.
    let receivePassAnim seat dir (handView : HandView) cardViews =

            // get incoming card backs
        let backs =
            let fromSeat = ExchangeDirection.unapply seat dir
            ExchangeView.finish fromSeat
        assert(backs |> Seq.forall CardView.isBack)

            // add incoming card fronts to hand view
        assert(cardViews |> Seq.forall (CardView.isBack >> not))
        handView.AddRange(cardViews)
        handView.Sort(fun viewA viewB ->
            let keyA = CardView.card viewA |> sortKey
            let keyB = CardView.card viewB |> sortKey
            compare keyA keyB)

        Animation.Serial [|

                // replace backs with fronts
            yield! Array.map2 (fun back front ->
                Animation.create back (ReplaceWith front))
                backs
                cardViews

                // wait
            yield Animation.Sleep 2000

                // move fronts into the hand
            yield HandView.dealAnim seat handView
        |]

    /// Animates the playing of a card from an open hand view.
    let playAnim seat (handView : HandView) (cardView : CardView) =

            // remove selected card from hand
        let flag = handView.Remove(cardView)
        assert(flag)

            // animate card being played
        let animPlay =
            [|
                BringToFront                       // bring card to front
                    |> Animation.create cardView
                TrickView.playAnim seat cardView   // slide revealed card to center
            |] |> Animation.Serial

            // animate adjustment of remaining cards to fill gap
        let animAdjust = HandView.adjustAnim seat handView

            // animate in parallel
        [|
            animPlay
            animAdjust
        |] |> Animation.Parallel
