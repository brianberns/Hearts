namespace Hearts

open PlayingCards

/// Direction in which cards are passed prior to playout.
type ExchangeDirection =
    | Left = 0
    | Right = 1
    | Across = 2
    | Hold = 3

module ExchangeDirection =

    /// Total number of exchange directions.
    let numDirections =
        Enum.getValues<ExchangeDirection>.Length

    /// Applies the given exchange direction to the given seat.
    let apply seat (dir : ExchangeDirection) =
        let n =
            match dir with
                | ExchangeDirection.Hold -> 0
                | ExchangeDirection.Left -> 1
                | ExchangeDirection.Across -> 2
                | ExchangeDirection.Right -> 3
                | _ -> failwith "Unexpected"
        seat |> Seat.incr n

/// Cards passed from one player to another.
type Exchange =
    {
        /// Dealer passes last.
        Dealer : Seat

        /// Card exchange direction.
        ExchangeDirection : ExchangeDirection

        /// Cards passed by each player in reverse chronological
        /// order.
        Passes : List<Set<Card>>
    }

module Exchange =

    /// Creates an empty exchange.
    let create dealer dir =
        {
            Dealer = dealer
            ExchangeDirection = dir
            Passes = List.empty
        }

    /// No exchange?
    let isHold exchange =
        exchange.ExchangeDirection = ExchangeDirection.Hold

    /// Number of cards passed by each player.
    let numCards = 3

    /// Whose turn is it to pass cards in the given exchange?
    let currentPasser exchange =
        assert(exchange |> isHold |> not)
        assert(exchange.Passes.Length < Seat.numSeats)
        exchange.Dealer
            |> Seat.incr (exchange.Passes.Length + 1)

    /// Adds current passer's cards to the given exchange.
    let addPass cards exchange =
        assert(exchange |> isHold |> not)
        assert(exchange.Passes.Length < Seat.numSeats)
        assert(cards |> Set.count = numCards)
        {
            exchange with
                Passes = cards :: exchange.Passes
        }

    /// Cards passed by each player in the given exchange, in
    /// chronological order.
    let seatPasses exchange =
        assert(exchange |> isHold |> not)
        let seats = exchange.Dealer.Next |> Seat.cycle
        let passes = exchange.Passes |> List.rev
        Seq.zip seats passes

    /// An exchange is complete when it is a hold hand, or all
    /// players have passed cards.
    let isComplete exchange =
        assert(
            exchange |> isHold |> not
                || exchange.Passes.IsEmpty)
        isHold exchange
            || exchange.Passes.Length = Seat.numSeats
