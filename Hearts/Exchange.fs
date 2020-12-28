namespace Hearts

open PlayingCards

/// Direction in which cards are passed prior to playout.
type ExchangeDirection =
    | Left = 1
    | Right = -1
    | Across = 2
    | Hold = 0

module ExchangeDirection =

    /// Next exchange direction, according to Hearts rules (not numerically).
    let next = function
        | ExchangeDirection.Left -> ExchangeDirection.Right
        | ExchangeDirection.Right -> ExchangeDirection.Across
        | ExchangeDirection.Across -> ExchangeDirection.Hold
        | ExchangeDirection.Hold -> ExchangeDirection.Left
        | _ -> failwith "Unexpected"

    /// Applies the given exchange direction to the given seat.
    let apply seat (dir : ExchangeDirection) =
        assert(dir <> ExchangeDirection.Hold)
        seat |> Seat.incr (int dir)

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

    /// Number of cards passed by each player.
    let numCards = 3

    /// Whose turn is it to pass cards in the given exchange?
    let currentPasser exchange =
        assert(exchange.ExchangeDirection <> ExchangeDirection.Hold)
        assert(exchange.Passes.Length < Seat.numSeats)
        exchange.Dealer
            |> Seat.incr (exchange.Passes.Length + 1)

    /// Adds current passer's cards to the given exchange.
    let addPass cards exchange =
        assert(exchange.ExchangeDirection <> ExchangeDirection.Hold)
        assert(exchange.Passes.Length < Seat.numSeats)
        assert(cards |> Set.count = numCards)
        {
            exchange with
                Passes = cards :: exchange.Passes
        }

    /// Cards passed by each player in the given exchange, in
    /// chronological order.
    let seatPasses exchange =
        assert(exchange.ExchangeDirection <> ExchangeDirection.Hold)
        let seats = exchange.Dealer.Next |> Seat.cycle
        let passes = exchange.Passes |> List.rev
        Seq.zip seats passes

    /// Have all players contributed cards to the given exchange?
    let isComplete exchange =
        assert(exchange.ExchangeDirection <> ExchangeDirection.Hold)
        exchange.Passes.Length = Seat.numSeats
