namespace Hearts.Web.Client

module Remoting =

    open Fable.Remoting.Client
    open Hearts.Web

    /// Prefix routes with /Hearts.
    let routeBuilder typeName methodName = 
        sprintf "/Hearts/%s/%s" typeName methodName

    /// Server API.
    let api =
        Remoting.createApi()
            |> Remoting.withRouteBuilder routeBuilder
            |> Remoting.buildProxy<IHeartsApi>

    /// Chooses an action for the given info set.
    let getActionIndex infoSet =
        async {
            match! Async.Catch(api.GetActionIndex infoSet) with
                | Choice1Of2 index -> return index
                | Choice2Of2 exn ->
                    failwith exn.Message   // is there a better way to handle this?
                    return -1
        }

    /// Gets the strategy for the given info set.
    let getStrategy infoSet =
        async {
            match! Async.Catch(api.GetStrategy infoSet) with
                | Choice1Of2 strategy -> return strategy
                | Choice2Of2 exn ->
                    failwith exn.Message   // is there a better way to handle this?
                    return Array.empty
        }

/// Plays Hearts by calling a remote server.
module WebPlayer =

    open Hearts

    /// Plays a card in the given deal.
    let makePlay deal =

            // get legal plays in this situation
        let infoSet = OpenDeal.currentInfoSet deal
        let legalPlays =
            InformationSet.legalPlays infoSet
                |> Seq.toArray

            // choose play
        match legalPlays.Length with
            | 0 -> failwith "Unexpected"
            | 1 -> async { return legalPlays[0] }
            | _ ->
                async {
                    let! iAction =
                        Remoting.getActionIndex infoSet
                    return legalPlays[iAction]
                }
