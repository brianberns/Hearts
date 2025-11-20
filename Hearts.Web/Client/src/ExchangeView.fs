namespace Hearts.Web.Client

open PlayingCards
open Hearts

module ExchangeView =

    /// Cards passed in the current exchange.
    /// ASSUMPTION: Only one exchange view per app.
    let private cardViewMap =
        Enum.getValues<Seat>
            |> Seq.map (fun seat ->
                seat,   // from seat
                ResizeArray<CardView>(Pass.numCards))   // MUTABLE
            |> dict

    /// Center position of a card being passed to a seat.
    let private passPosMap =
        Position.seatMap [
            Seat.West,  (20, 69)
            Seat.North, (28, 15)
            Seat.East,  (80, 31)
            Seat.South, (72, 84)
        ]

    /// Gets the target pass position for the given seat
    /// passing in the given direction.
    let private getPassPosition fromSeat dir n delta =

            // horizontal offset (-1, 0, 1)
        let offset =
(*
            assert(Pass.numCards = 3)
            float (n - 1) * delta
*)
            assert(Pass.numCards = 1)
            0

        let toSeat =
            ExchangeDirection.apply fromSeat dir
        passPosMap[toSeat]
            + Position.ofFloats(offset, 0.0)

    /// Animates a card being passed by the given seat.
    let passAnim seat dir cardView delta =

            // get pass target position
        let n = cardViewMap[seat].Count
        let pos = getPassPosition seat dir n delta

            // add card view
        cardViewMap[seat].Add(cardView)

            // animate playing the card
        AnimationAction.moveTo pos
            |> Animation.create cardView

    /// Finishes an exchange for the given sending seat.
    let finish fromSeat =

            // find cards passed by the given seat
        let cardViews = cardViewMap[fromSeat]
        assert(cardViews.Count = Pass.numCards)

            // make a copy
        let copy = Seq.toArray cardViews

            // reset to a new exchange
        cardViewMap[fromSeat].Clear()

        copy
