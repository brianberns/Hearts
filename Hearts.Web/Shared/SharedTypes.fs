namespace Hearts.Web

type IHeartsApi =
    {
        GetPlayIndex : string (*key*) -> Async<Option<int>> (*action index*)
    }
