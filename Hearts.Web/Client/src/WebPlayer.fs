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
    let getActionIndex key =
        async {
            match! Async.Catch(api.GetPlayIndex(key)) with
                | Choice1Of2 indexOpt -> return indexOpt
                | Choice2Of2 exn ->
                    failwith exn.Message   // is there a better way to handle this?
                    return None
        }

    /// Gets the strategy for the given info set.
    let getStrategy key =
        async {
            match! Async.Catch(api.GetStrategy(key)) with
                | Choice1Of2 strategyOpt -> return strategyOpt
                | Choice2Of2 exn ->
                    failwith exn.Message   // is there a better way to handle this?
                    return None
        }

/// Plays Hearts by calling a remote server.
module WebPlayer =

    open Hearts
    open Hearts.FastCfr

    /// Adjusts the given trick.
    let private adjustTrick adjust trick =
        {
            trick with
                Leader = adjust trick.Leader
                HighPlayOpt =
                    trick.HighPlayOpt
                        |> Option.map (fun (seat, card) ->
                            adjust seat, card)
        }

    /// Adjusts the given deal so the given seat is the current
    /// player.
    let private adjustDeal seat deal =
        let adjust =
            deal
                |> ClosedDeal.currentPlayer
                |> Seat.getIndex seat
                |> Seat.incr
        {
            deal with
                CurrentTrickOpt =
                    deal.CurrentTrickOpt
                        |> Option.map (fun trick ->
                            let trick = adjustTrick adjust trick
                            assert(Trick.currentPlayer trick = seat)
                            trick)
                CompletedTricks =
                    deal.CompletedTricks
                        |> List.map (adjustTrick adjust)
                Voids =
                    deal.Voids
                        |> Set.map (fun (seat, suit) ->
                            adjust seat, suit)
                Score =
                    {
                        ScoreMap =
                            deal.Score.ScoreMap
                                |> Map.toSeq
                                |> Seq.map (fun (seat, points) ->
                                    adjust seat, points)
                                |> Map
                    }
        }

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
                    let infoSetKey =
                        deal.ClosedDeal
                            |> adjustDeal Seat.South
                            |> GameState.getInfoSetKey hand
                    let! iActionOpt =
                        Remoting.getActionIndex infoSetKey
                    return
                        iActionOpt
                            |> Option.map (Array.get legalPlays)
                            |> Option.defaultValue legalPlays[0]
                }
