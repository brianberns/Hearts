namespace Hearts.Web

open PlayingCards
open Hearts

type IHeartsApi =
    {
        /// Chooses an action for the given info set.
        GetPlayIndex : Hand -> ClosedDeal -> Async<int> (*action index*)

        /// Gets the strategy for the given info set.
        GetStrategy : Hand -> ClosedDeal -> Async<float[]> (*action probabilities*)
    }
