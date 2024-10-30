namespace Hearts.Web

type IHeartsApi =
    {
        GetPlayIndex : string (*key*) -> Async<Option<int>> (*action index*)
        GetStrategy : string (*key*) -> Async<Option<float[]>> (*action probabilities*)
    }
