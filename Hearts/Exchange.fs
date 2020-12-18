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

/// Exchange phase of a deal.
type Exchange =
    {
        /// Direction of this exchange.
        Direction : ExchangeDirection

        /// Cards received by each player.
        CardMap : Map<Seat, Set<Card>>
    }

module Exchange =

    /// Creates an empty exchange in the given direction.
    let create dir =
        {
            Direction = dir
            CardMap = Map.empty
        }

    /// Number of cards passed by each player.
    let numCards = 3

    /// Passes the given cards to the given player.
    let pass recevier cards exchange =
        assert(cards |> Set.count = numCards)
        {
            exchange with
                CardMap =
                    exchange.CardMap
                        |> Map.add recevier cards
        }

    /// Cards passed to the given player in the given exchange.
    let cardsReceived seat exchange =
        match exchange.Direction with
            | ExchangeDirection.Hold ->
                assert(exchange.CardMap.IsEmpty)
                Set.empty
            | _ -> exchange.CardMap.[seat]

    /// Cards passed by the given player in the given exchange.
    let cardsPassed seat exchange =
        let receiver =
            exchange.Direction
                |> ExchangeDirection.apply seat
        cardsReceived receiver exchange
