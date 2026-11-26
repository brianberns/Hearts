namespace Hearts.Experiments

open System
open System.Collections.Concurrent
open System.Diagnostics
open System.IO
open System.Threading.Tasks

/// W computed exactly for a game whose deals are independent draws from one distribution of deal scores,
/// such as Hearts.Generate's GameScore model, or the deals of WData's games. Under the arena's rules
/// (moon shots add 26 to each opponent; a tie for low at 100 or more plays on), every deal adds 26 or 78
/// points, so W at a score is a sum over the next deal's outcomes of W at higher scores, and one pass from
/// the highest scores down computes it with no sampling. A distribution symmetric under every permutation of
/// the seats makes W symmetric too, so the table holds each sorted score only.
module WExact =

    /// One outcome of a deal: each seat's points, and its probability.
    type Outcome = { Points : int[]; Prob : float }

    let private numSeats = 4
    let private threshold = 100

    /// Merges outcomes with the same points.
    let private merge (outcomes : seq<int[] * float>) =
        outcomes
            |> Seq.groupBy (fun (points, _) -> String.Join(",", points))
            |> Seq.map (fun (_, group) ->
                { Points = fst (Seq.head group); Prob = group |> Seq.sumBy snd })
            |> Seq.toArray

    /// x (x + 1) ... (x + n - 1).
    let private rising (x : float) n =
        Seq.fold (fun acc k -> acc * (x + float k)) 1.0 (seq { 0 .. n - 1 })

    let private factorial n = rising 1.0 n

    /// The deal scores of Hearts.Generate/GameScore.fs, whose constants these copy: a moon shot 18.5% of
    /// the time; otherwise the Q♠ to a random seat, with some hearts, and the remaining hearts to the other
    /// three by a Pólya urn, whose chance of each split is the Dirichlet-multinomial's.
    let gameScoreOutcomes () =
        let shootRate = 0.185
        let queenHeartWeights =
            [| 118.0; 94.0; 84.0; 94.0; 91.0; 81.0; 74.0; 70.0; 69.0; 65.0; 56.0; 60.0; 42.0 |]
        let alpha = 0.4
        let totalWeight = Array.sum queenHeartWeights
        merge [|
            for shooter in 0 .. 3 ->
                Array.init numSeats (fun i -> if i = shooter then 0 else 26), shootRate / 4.0
            for queenSeat in 0 .. 3 do
                for nQueenHearts in 0 .. 12 do
                    let r = 13 - nQueenHearts
                    for c1 in 0 .. r do
                        for c2 in 0 .. r - c1 do
                            let c3 = r - c1 - c2
                            let counts = [| c1; c2; c3 |]
                            let split =
                                factorial r / (counts |> Array.map factorial |> Array.reduce (*))
                                    * (counts |> Array.map (rising alpha) |> Array.reduce (*))
                                    / rising (3.0 * alpha) r
                            let points =
                                Array.init numSeats (fun seat ->
                                    let offset = (seat - queenSeat + 4) % 4
                                    if offset = 0 then 13 + nQueenHearts else counts[offset - 1])
                            yield points,
                                (1.0 - shootRate) / 4.0 * queenHeartWeights[nQueenHearts] / totalWeight * split
        |]

    /// One deal at its start, from WData.
    type Row = { Scores : int[]; Winner : int; Game : int }

    let readRows (path : string) =
        File.ReadLines(path)
            |> Seq.skip 1
            |> Seq.map (fun line ->
                let xs = line.Split(',') |> Array.map int
                xs[0 .. 3], xs[5])
            |> Seq.mapFold (fun game (scores, winner) ->
                let game = if Array.forall ((=) 0) scores then game + 1 else game   // each game starts at zero
                { Scores = scores; Winner = winner; Game = game }, game) -1
            |> fst
            |> Seq.toArray

    /// Every permutation of the seats.
    let private permutations =
        let rec perms (xs : int list) =
            match xs with
                | [] -> [ [] ]
                | _ -> [ for x in xs do for rest in perms (List.except [x] xs) -> x :: rest ]
        perms [ 0 .. 3 ] |> List.map Array.ofList |> Array.ofList

    /// The deals of WData's games, taken as each starting score's difference from the next deal's in the
    /// same game, and made symmetric over every permutation of the seats. (Each game's last deal is missing,
    /// since WData records only starting scores.)
    let private empiricalOutcomes (rows : Row[]) =
        let deals =
            rows
                |> Array.pairwise
                |> Array.filter (fun (r1, r2) -> r1.Game = r2.Game)
                |> Array.map (fun (r1, r2) -> Array.map2 (-) r2.Scores r1.Scores)
        let prob = 1.0 / float (deals.Length * permutations.Length)
        merge [|
            for deal in deals do
                for perm in permutations ->
                    Array.init numSeats (fun i -> deal[perm[i]]), prob
        |], deals.Length

    /// Binomial coefficients, for indexing sorted scores.
    let binom =
        Array2D.init 110 5 (fun n k ->
            if k > n then 0
            else int (Seq.fold (fun acc i -> acc * int64 (n - i) / int64 (i + 1)) 1L (seq { 0 .. k - 1 })))

    /// The index of sorted scores a <= b <= c <= d < 100 (the combinatorial number system).
    let inline index a b c d =
        binom[a, 1] + binom[b + 1, 2] + binom[c + 2, 3] + binom[d + 3, 4]

    let numStates = binom[threshold + 3, 4]

    /// Sorts four scores, answering the seats in order of their scores.
    let private sortSeats (t : int[]) =
        let order = [| 0; 1; 2; 3 |]
        Array.sortInPlaceWith (fun i j -> compare t[i] t[j]) order
        order

    /// Each sorted state's chance of winning, by position in sorted order.
    type private Table =
        {
            Values : float[]
            Outcomes : Outcome[]
            Ties : ConcurrentDictionary<int64, float[]>
        }

    /// Answers W at any scores (seats in any fixed order), adding p times it to acc.
    let rec private accumulate (table : Table) (t : int[]) p (acc : float[]) =
        let order = sortSeats t
        let low = t[order[0]]
        let high = t[order[3]]
        if high < threshold then
            let i = 4 * index t[order[0]] t[order[1]] t[order[2]] t[order[3]]
            for k in 0 .. 3 do
                acc[order[k]] <- acc[order[k]] + p * table.Values[i + k]
        elif t[order[1]] > low then
            acc[order[0]] <- acc[order[0]] + p   // game over, won outright
        else
                // tie for low at the end: play continues
            let sorted = Array.init numSeats (fun k -> t[order[k]])
            let key = sorted |> Array.fold (fun key x -> (key <<< 16) ||| int64 x) 0L
            let values = table.Ties.GetOrAdd(key, (fun (_ : int64) -> tieValues table sorted))
            for k in 0 .. 3 do
                acc[order[k]] <- acc[order[k]] + p * values[k]

    /// W at sorted scores with a tie for low at the end, by position.
    and private tieValues (table : Table) (sorted : int[]) =
        if Array.sum sorted > 700 then
                // a tie that has persisted for many deals: share it (this almost never happens)
            let n = sorted |> Array.filter ((=) sorted[0]) |> Array.length
            Array.init numSeats (fun k -> if sorted[k] = sorted[0] then 1.0 / float n else 0.0)
        else
            let acc = Array.zeroCreate numSeats
            for o in table.Outcomes do
                accumulate table (Array.map2 (+) sorted o.Points) o.Prob acc
            acc

    /// Computes the table.
    let private compute (outcomes : Outcome[]) =
        let table = { Values = Array.zeroCreate (4 * numStates); Outcomes = outcomes; Ties = ConcurrentDictionary() }
        let bySum =
            [| for d in 0 .. threshold - 1 do
                for c in 0 .. d do
                    for b in 0 .. c do
                        for a in 0 .. b -> [| a; b; c; d |] |]
                |> Array.groupBy Array.sum
                |> Array.sortByDescending fst
        let stopwatch = Stopwatch.StartNew()
        for sum, states in bySum do
            Parallel.ForEach(states, fun (s : int[]) ->
                let acc = Array.zeroCreate numSeats
                let t = Array.zeroCreate numSeats
                for o in outcomes do
                    for k in 0 .. 3 do t[k] <- s[k] + o.Points[k]
                    accumulate table t o.Prob acc
                let i = 4 * index s[0] s[1] s[2] s[3]
                Array.blit acc 0 table.Values i numSeats) |> ignore
            if sum % 50 = 0 then
                printfn $"   sum {sum}: {stopwatch.Elapsed}"
        table

    /// W at the given scores, by seat.
    let private w table (scores : int[]) =
        let acc = Array.zeroCreate numSeats
        accumulate table scores 1.0 acc
        acc

    /// Mean cross-entropy of W on the given deals.
    let private loss table (rows : Row[]) =
        rows |> Array.averageBy (fun row -> -log ((w table row.Scores)[row.Winner]))

    /// Calibration on the given deals, by tenths.
    let private calibrate table (rows : Row[]) =
        let buckets = Array.init 10 (fun _ -> ResizeArray<float * bool>())
        for row in rows do
            let ps = w table row.Scores
            for seat in 0 .. 3 do
                buckets[min 9 (int (ps[seat] * 10.0))].Add(ps[seat], row.Winner = seat)
        printfn "   calibration (predicted vs actual win rate, validation):"
        for i, b in Seq.indexed buckets do
            if b.Count > 0 then
                let predicted = b |> Seq.averageBy fst
                let actual = b |> Seq.averageBy (fun (_, won) -> if won then 1.0 else 0.0)
                printfn $"     %.1f{float i / 10.0}-%.1f{float (i + 1) / 10.0}: predicted %.3f{predicted}, actual %.3f{actual}  ({b.Count})"

    /// Summarizes a distribution of deal scores.
    let private describe (outcomes : Outcome[]) =
        let shoot = outcomes |> Array.filter (fun o -> Array.sum o.Points = 78) |> Array.sumBy (fun o -> o.Prob)
        let sd =
            outcomes
                |> Array.filter (fun o -> Array.sum o.Points = 26)
                |> Array.sumBy (fun o -> o.Prob * (float o.Points[0] - 6.5) ** 2.0)
        printfn $"   {outcomes.Length} distinct outcomes, total probability %.6f{outcomes |> Array.sumBy (fun o -> o.Prob)}, moon shots %.4f{shoot}, s.d. of a seat's points without a shoot %.3f{sqrt (sd / (1.0 - shoot))}"

    /// Computes W from the given deal model ("gamescore" or "empirical", the deals of the data), scores it on
    /// the data's games, and saves the table of sorted scores (float32, four per state) if asked.
    let run (dataPath : string) (model : string) (outPath : string option) =
        Process.GetCurrentProcess().PriorityClass <- ProcessPriorityClass.BelowNormal
        let rows = readRows dataPath
        let validRows = rows |> Array.filter (fun r -> r.Game % 10 = 0)
        printfn $"{(rows |> Array.last).Game + 1} games, {rows.Length} deals, {validRows.Length} for validation (as in wfit)"
        let outcomes =
            match model with
                | "gamescore" -> gameScoreOutcomes ()
                | "empirical" ->
                    let outcomes, numDeals = empiricalOutcomes rows
                    printfn $"   {numDeals} deals from the data"
                    outcomes
                | _ -> failwith $"unknown deal model: {model}"
        printfn $"deal model: {model}"
        describe outcomes
        let stopwatch = Stopwatch.StartNew()
        let table = compute outcomes
        printfn $"{numStates} states and {table.Ties.Count} tie states computed in {stopwatch.Elapsed}"
        printfn $"uniform guess: %.5f{log 4.0}"
        printfn $"   loss on validation games %.5f{loss table validRows}, on all games %.5f{loss table rows}"
        calibrate table validRows
        let show scores =
            let label = String.Join(",", scores |> Array.map string)
            let p = String.Join(" ", w table scores |> Array.map (fun x -> x.ToString("0.000")))
            printfn $"   %-16s{label}: {p}"
        printfn "   W (scores in playing order):"
        show [| 0; 0; 0; 0 |]
        show [| 13; 0; 0; 0 |]
        show [| 50; 50; 50; 50 |]
        show [| 63; 50; 50; 50 |]
        show [| 80; 70; 70; 70 |]
        show [| 93; 70; 70; 70 |]
        show [| 60; 90; 75; 80 |]
        match outPath with
            | Some path ->
                use writer = new BinaryWriter(File.Create(path))
                for v in table.Values do writer.Write(float32 v)
                printfn $"saved {numStates} sorted states to {path}"
            | None -> ()
