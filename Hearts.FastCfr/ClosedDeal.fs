namespace Hearts.FastCfr

open Hearts

module ClosedDeal =

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
    let adjustDeal seat deal =
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
