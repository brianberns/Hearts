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

    let getActionIndex key =
        async {
            match! Async.Catch(api.GetPlayIndex(key)) with
                | Choice1Of2 indexOpt -> return indexOpt
                | Choice2Of2 exn ->
                    failwith exn.Message   // is there a better way to handle this?
                    return None
        }

/// Plays Hearts by calling a remote server.
module WebPlayer =

    open Hearts
    open Hearts.Cfrm

    /// Plays a card in the given deal.
    let makePlay (deal : OpenDeal) =

            // get legal plays in this situation
        let hand = OpenDeal.currentHand deal
        let legalPlays =
            ClosedDeal.legalPlays hand deal.ClosedDeal
                |> Seq.toArray

            // choose play
        match legalPlays.Length with
            | 0 -> failwith "Unexpected"
            | 1 -> async { return legalPlays[0] }
            | _ ->
                async {
                    let key = GameState.getKey deal
                    match! Remoting.getActionIndex key with
                        | Some iAction -> return legalPlays[iAction]
                        | None -> return legalPlays[0]
                }
