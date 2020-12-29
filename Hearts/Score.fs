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
    let private zeroMap =
        Seat.allSeats
            |> Seq.map (fun seat -> seat, 0)
            |> Map.ofSeq

    /// Initial score.
    let zero =
        ScoreMap zeroMap

    /// Creates a score for the given seat.
    let create seat points =
        zeroMap
            |> Map.add seat points
            |> ScoreMap

    /// Sum of all points in the given score.
    let sum (ScoreMap scoreMap) =
        scoreMap
            |> Map.toSeq
            |> Seq.sumBy snd
