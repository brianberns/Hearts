namespace Hearts

open PlayingCards

/// A deal is a round of play within a game. A closed deal contains
/// no information about unplayed cards, which are kept private by
/// each player. Cards played during a deal are grouped into tricks.
type ClosedDeal =
    {
        /// Current active trick, if the deal is not yet complete.
        CurrentTrickOpt : Option<Trick>

        /// Completed tricks, in reverse chronological order.
        CompletedTricks : List<Trick>

        /// Suits that players are known to be void in.
        Voids : Set<Seat * Suit>
    }
