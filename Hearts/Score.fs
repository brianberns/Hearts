namespace Hearts

/// Points scored by each player during one or more deals.
type Score =
    | ScoreMap of Map<Seat, int>

    /// Adds two scores.
    static member (+) (ScoreMap mapA, ScoreMap mapB) =
        Seat.allSeats
            |> Seq.map (fun seat ->
                let sum = mapA.[seat] + mapB.[seat]
                seat, sum)
            |> Map
            |> ScoreMap

module Score =

    /// Initial score.
    let zero =
        Seat.allSeats
            |> Seq.map (fun seat -> seat, 0)
            |> Map.ofSeq
            |> ScoreMap
