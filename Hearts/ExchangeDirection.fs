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
    let apply seat dir =
        let n =
            match dir with
                | ExchangeDirection.Hold -> 0
                | ExchangeDirection.Left -> 1
                | ExchangeDirection.Across -> 2
                | ExchangeDirection.Right -> 3
                | _ -> failwith "Unexpected"
        seat |> Seat.incr n

    /// Finds the seat that passes cards to the given seat.
    let unapply seat dir =
        let n =
            match dir with
                | ExchangeDirection.Hold -> 0
                | ExchangeDirection.Left -> 3
                | ExchangeDirection.Across -> 2
                | ExchangeDirection.Right -> 1
                | _ -> failwith "Unexpected"
        seat |> Seat.incr n
