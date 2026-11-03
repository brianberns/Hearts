namespace Hearts.Heuristic.Killer

/// Killer Hearts' model of the opponents' hands (its opp_hands), from one seat's point of view. Each
/// cell says whether an opponent holds a card:
///    2  known to hold it
///    1  inferred to hold it
///    0  nothing known
///   -1  inferred not to hold it
///   -2  known not to hold it
/// Suit lengths are kept in step with the cells: the number of cards each opponent may hold in a
/// suit (cells >= 0), is inferred to hold (>= 1), and is known to hold (= 2), and the number of cards
/// it is known not to hold (= -2, counted as cells become -2).
type OppHands(me : KSeat) =
    let cells = Array3D.zeroCreate<int> 4 15 4
    let suitLenPoss = Array2D.zeroCreate<int> 4 4
    let suitLenInf = Array2D.zeroCreate<int> 4 4
    let suitLenFirm = Array2D.zeroCreate<int> 4 4
    let handLen = Array.zeroCreate<int> 4
    let cardsNotHeld = Array2D.zeroCreate<int> 4 4

    /// The opponents, in playing order after this seat.
    let opponents = [ for k in 1 .. 3 -> (me + k) % 4 ]

    member _.Item
        with get (p, r, s) = cells[p, r, s]
        and set (p, r, s) value = cells[p, r, s] <- value

    member _.SuitLenPoss = suitLenPoss
    member _.SuitLenInf = suitLenInf
    member _.SuitLenFirm = suitLenFirm
    member _.HandLen = handLen
    member _.CardsNotHeld = cardsNotHeld
    member _.Opponents = opponents

    /// Counts the suit lengths from the cells, once they are first filled in.
    member _.CountSuitLengths() =
        for p in opponents do
            for s in allSuits do
                for r in 2 .. ace do
                    let k = cells[p, r, s]
                    if k >= 0 then
                        suitLenPoss[p, s] <- suitLenPoss[p, s] + 1
                        if k >= 1 then
                            suitLenInf[p, s] <- suitLenInf[p, s] + 1
                            if k = 2 then suitLenFirm[p, s] <- suitLenFirm[p, s] + 1
                    elif k = -2 then cardsNotHeld[p, s] <- cardsNotHeld[p, s] + 1
                handLen[p] <- handLen[p] + suitLenInf[p, s]

    /// Is the opponent void in the suit, as far as this seat can tell? (player_void_in)
    member _.IsVoid(p, s) = p >= 0 && suitLenPoss[p, s] = 0

    /// Sets an opponent's cell for a card (update_opp_hands). A known cell (2 or -2) never changes.
    /// Unless allowed, an inferred holding (1) does not become an inferred absence (-1). Giving one
    /// opponent the card makes the others unlikely (-1), or certain (-2) not to hold it, and
    /// knowing that no other opponent can hold a card gives it to the last one.
    member this.Update(code, p, r, s, allowOneToMinusOne) =
        let k = cells[p, r, s]
        if code = k then ()
        elif k = 2 || k = -2 then ()
        elif k = 1 && code = -1 && not allowOneToMinusOne then ()
        else
            let newValue = if code = 2 then -2 else -1
            cells[p, r, s] <- code
            if code >= 0 then
                for i in opponents do
                    if i <> p then
                        let j = cells[i, r, s]
                        if j < 0 then
                            if code = 2 then
                                if j > -2 then cardsNotHeld[i, s] <- cardsNotHeld[i, s] + 1
                                cells[i, r, s] <- -2
                        elif code >= 1 && j < 2 then
                            cells[i, r, s] <- newValue
                            if newValue = -2 then cardsNotHeld[i, s] <- cardsNotHeld[i, s] + 1
                            suitLenPoss[i, s] <- suitLenPoss[i, s] - 1
                            if j = 1 then
                                suitLenInf[i, s] <- suitLenInf[i, s] - 1
                                handLen[i] <- handLen[i] - 1
                if code > k then
                    if k < 0 then
                        suitLenPoss[p, s] <- suitLenPoss[p, s] + 1
                        if code > 0 then
                            suitLenInf[p, s] <- suitLenInf[p, s] + 1
                            handLen[p] <- handLen[p] + 1
                            if code = 2 then suitLenFirm[p, s] <- suitLenFirm[p, s] + 1
                    elif k = 0 then
                        suitLenInf[p, s] <- suitLenInf[p, s] + 1
                        handLen[p] <- handLen[p] + 1
                        if code = 2 then suitLenFirm[p, s] <- suitLenFirm[p, s] + 1
                    elif k = 1 then
                        suitLenFirm[p, s] <- suitLenFirm[p, s] + 1
                elif code < k then
                    suitLenInf[p, s] <- suitLenInf[p, s] - 1
                    handLen[p] <- handLen[p] - 1
            elif code = -1 then
                suitLenPoss[p, s] <- suitLenPoss[p, s] - 1
                if k > 0 then
                    suitLenInf[p, s] <- suitLenInf[p, s] - 1
                    handLen[p] <- handLen[p] - 1
            else
                cardsNotHeld[p, s] <- cardsNotHeld[p, s] + 1
                if k >= 0 then suitLenPoss[p, s] <- suitLenPoss[p, s] - 1
                if k > 0 then
                    suitLenInf[p, s] <- suitLenInf[p, s] - 1
                    handLen[p] <- handLen[p] - 1
                // if only one other opponent may hold the card, it does
                let candidates = opponents |> List.filter (fun i -> i <> p && cells[i, r, s] > -2)
                match candidates with
                    | [ m ] ->
                        let n = cells[m, r, s]
                        cells[m, r, s] <- 2
                        if n < 0 then suitLenPoss[m, s] <- suitLenPoss[m, s] + 1
                        if n <= 0 then
                            suitLenInf[m, s] <- suitLenInf[m, s] + 1
                            handLen[m] <- handLen[m] + 1
                        suitLenFirm[m, s] <- suitLenFirm[m, s] + 1
                    | _ -> ()

    /// Sets an opponent's cells for a run of ranks in a suit.
    member this.UpdateRun(code, p, lo, hi, s, allowOneToMinusOne) =
        for r in lo .. hi do this.Update(code, p, r, s, allowOneToMinusOne)

    /// The highest rank the opponent may hold in the suit, if any.
    member _.HighestRank(p, s) =
        seq { ace .. -1 .. 2 } |> Seq.tryFind (fun r -> cells[p, r, s] >= 0) |> Option.defaultValue none

    /// The lowest rank the opponent may hold in the suit, if any.
    member _.LowestRank(p, s) =
        seq { 2 .. ace } |> Seq.tryFind (fun r -> cells[p, r, s] >= 0) |> Option.defaultValue none
