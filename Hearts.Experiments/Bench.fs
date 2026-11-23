namespace Hearts.Experiments

open System
open System.Collections.Concurrent
open System.Diagnostics
open System.Threading
open System.Threading.Tasks

open PlayingCards
open Hearts
open Hearts.Heuristic
open Hearts.Model

/// Times the advantage model at various degrees of parallelism, and checks that it gives the same
/// strategies in parallel as sequentially.
module Bench =

    /// Plays the given number of games, the model at South against three Claude, at the given degree of
    /// parallelism (1 for sequential), and answers the final scores and the model's strategy at each info set.
    let private play (model : AdvantageModel) numGames degree =
        let strategies = ConcurrentDictionary<InformationSet, float32[]>(HashIdentity.Structural)
        let mutable decisions = 0
        let mutable modelTicks = 0L
        let player =
            let act (infoSet : InformationSet) =
                let start = Stopwatch.GetTimestamp()
                let strategy = Strategy.getFromAdvantage model [| infoSet |] |> Array.exactlyOne
                Interlocked.Add(&modelTicks, Stopwatch.GetTimestamp() - start) |> ignore
                Interlocked.Increment(&decisions) |> ignore
                strategies[infoSet] <- strategy.ToArray()
                    // a random choice seeded by the info set, so that every run plays the same games
                let rng = SplitMixRandom(SplitMixRandom.GetSeed(0, hash infoSet))
                strategy |> Vector.sample rng |> Array.get infoSet.LegalActions
            { Act = act }
        let playerMap =
            Enum.getValues<Seat>
                |> Seq.map (fun seat -> seat, (if seat = Seat.South then player else Claude.player))
                |> Map
        let playOne iGame =
            let rng = SplitMixRandom(SplitMixRandom.GetSeed(0, iGame))
            Game.playGame Rules.arena rng playerMap (enum<Seat> (iGame % Seat.numSeats))
        let watch = Stopwatch.StartNew()
        let scores = Array.zeroCreate numGames
        let options = ParallelOptions(MaxDegreeOfParallelism = degree)
        Parallel.For(0, numGames, options, fun i -> scores[i] <- playOne i) |> ignore
        let wall = watch.Elapsed.TotalSeconds
        let perThread = float decisions / (float modelTicks / float Stopwatch.Frequency)
        printfn $"%2d{degree} threads: {numGames} games in %.1f{wall} s; {decisions} model decisions, %.0f{perThread}/s per thread"
        scores, strategies

    /// Runs the benchmark.
    let run modelPath numGames torchThreads =
        torchThreads |> Option.iter TorchSharp.torch.set_num_threads
        printfn $"{Environment.ProcessorCount} cores, {TorchSharp.torch.get_num_threads ()} torch threads"
        use model = Players.loadModel modelPath
        let baseScores, baseStrategies = play model numGames 1
        for degree in [ 2; 4; 8; Environment.ProcessorCount ] do
            let scores, strategies = play model numGames degree
            let sameScores = Array.forall2 (fun (a : Score) (b : Score) -> a.Points = b.Points) baseScores scores
            let differing =
                baseStrategies
                    |> Seq.filter (fun kv ->
                        match strategies.TryGetValue kv.Key with
                            | true, v -> v <> kv.Value
                            | _ -> true)
                    |> Seq.length
            if not sameScores || differing > 0 || strategies.Count <> baseStrategies.Count then
                printfn $"   DIFFERS from sequential: same scores {sameScores}, {differing} strategies differ"
