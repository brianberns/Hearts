namespace Hearts.Web

open PlayingCards
open Hearts

type IHeartsApi =
    {
        /// Chooses an action for the given info set.
        GetActionIndex : InformationSet -> Async<int> (*action index*)

        /// Gets the strategy for the given info set.
        GetStrategy : InformationSet -> Async<float[]> (*action probabilities*)
    }
