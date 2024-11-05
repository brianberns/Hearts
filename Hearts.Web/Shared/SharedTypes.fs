namespace Hearts.Web

type IHeartsApi =
    {
        /// Chooses an action for the given info set.
        GetPlayIndex : byte[] (*key*) -> Async<Option<int>> (*action index*)

        /// Gets the strategy for the given info set.
        GetStrategy : byte[] (*key*) -> Async<Option<float[]>> (*action probabilities*)
    }
