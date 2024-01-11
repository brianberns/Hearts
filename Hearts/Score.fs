namespace Hearts

open PlayingCards

/// Points taken by each player during one or more deals.
type Score =
    {
        ScoreMap : Map<Seat, int>
    }

    /// Number of points taken by the given player.
    member score.Item
        with get(seat) = score.ScoreMap[seat]

    /// Adds two scores.
    static member (+) (scoreA : Score, scoreB : Score) =
        {
            ScoreMap =
                Enum.getValues<Seat>
                    |> Seq.map (fun seat ->
                        let sum = scoreA[seat] + scoreB[seat]
                        seat, sum)
                    |> Map
        }

module Score =

    /// Initial score.
    let private zeroMap =
        Enum.getValues<Seat>
            |> Seq.map (fun seat -> seat, 0)
            |> Map

    /// Initial score.
    let zero = { ScoreMap = zeroMap }

    /// Creates a score for the given seat.
    let create seat points =
        {
            ScoreMap = Map.add seat points zeroMap
        }

    /// Sum of all points in the given score.
    let sum score =
        Seq.sum score.ScoreMap.Values
