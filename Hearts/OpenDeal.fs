namespace Hearts

open PlayingCards

type OpenDeal =
    {
        /// Cards exchanged before play begins, if any.
        Exchange : Exchange

        /// Base deal.
        ClosedDeal : ClosedDeal

        /// Each player's unplayed cards.
        UnplayedCardMap : Map<Seat, Set<Card>>
    }
