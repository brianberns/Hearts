namespace Hearts

open PlayingCards

/// Points taken by each player in a deal or game.
type Score =
    {
        /// Number of points taken by each player,
        /// indexed by seat.
        Points : int[]
    }

    /// Number of points taken by the given player.
    member score.Item
        with get(seat : Seat) = score.Points[int seat]

    /// Adds two scores.
    static member (+) (scoreA : Score, scoreB : Score) =
        {
            Points =
                Enum.getValues<Seat>
                    |> Array.map (fun seat ->
                        scoreA[seat] + scoreB[seat])
        }

module Score =

    /// Creates a score from the given per-seat points.
    let ofPoints points =
        { Points = points }

    /// Initial score.
    let zero =
        Array.zeroCreate Seat.numSeats
            |> ofPoints

    /// Creates a score for the given seat.
    let create (seat : Seat) points =
        Array.init Seat.numSeats (fun iSeat ->
            if iSeat = int seat then points
            else 0)
            |> ofPoints

    /// Indexes the given score by seat.
    let indexed score =
        score.Points
            |> Seq.indexed
            |> Seq.map (fun (iSeat, points) ->
                enum<Seat> iSeat, points)

    /// Sum of all points in the given score.
    let sum score =
        Seq.sum score.Points
