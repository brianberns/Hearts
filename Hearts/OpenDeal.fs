namespace Hearts

open PlayingCards

type ExchangeDirection =
    | Left
    | Right
    | Across

type Exchange =
    {
        /// Direction of this exchange.
        Direction : ExchangeDirection

        /// Cards passed to a player.
        CardsPassed : Set<Card>

        /// Cards received by a player.
        CardsReceived : Set<Card>
    }

type OpenDeal =
    {
        /// Cards exchanged before play begins, if any.
        ExchangeOpt : Option<Map<Seat, Exchange>>

        /// Base deal.
        ClosedDeal : ClosedDeal

        /// Each player's unplayed cards.
        UnplayedCardMap : Map<Seat, Set<Card>>
    }
