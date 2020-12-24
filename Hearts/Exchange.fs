namespace Hearts

open PlayingCards

/// Direction in which cards are passed prior to playout.
type ExchangeDirection =
    | Left = -1
    | Right = 1
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
        seat |> Seat.incr (int dir)

module Exchange =

    /// Number of cards passed by each player.
    let numCards = 3
