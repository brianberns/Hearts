namespace Hearts.ValidateSamples

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

open PlayingCards
open Hearts.Learn

/// The payoff a store's regrets were computed from.
type PayoffLevel =
    | DealLevel
    | GameLevel

/// Count and first examples of one category of finding.
type Tally =
    {
        Count : int64
        Examples : List<int64 * string>   // sample index, detail
    }

/// Validates advantage sample stores (Hearts.Generate's AdvantageSamples-*.bin) and reports what's
/// in them and what's wrong with them.
module Program =

    let private numExamples = 3
    let private samplesPerChunk = 50_000

    /// E.g. AdvantageSamples-Game-i003-41234.bin, or AdvantageSamples-i003-41234.bin before 2619a2e.
    let private nameRegex =
        Regex(@"^AdvantageSamples-(?:(Game|Deal)-)?i(\d{3,})-(\d+)\.bin$")

    /// Largest possible regret: a deal-level payoff runs from about -24.7 (25 points against 1/3 for
    /// each other seat) to +26 (a moon); a game-level payoff, a change in the chance of winning,
    /// from -1 to +1.
    let private regretBound = function
        | DealLevel -> 52f
        | GameLevel -> 2.001f

    /// Checks the given number of samples starting at the current position of the given stream, and
    /// folds their results into the given state.
    let private checkChunk (layout : Layout) level (stream : Stream) firstIndex count (tallies, stats) =
        let buf = Array.zeroCreate<byte> (count * layout.SampleBytes)
        stream.ReadExactly(buf, 0, buf.Length)
        let results =
            Array.Parallel.init count (fun i ->
                Check.check layout (regretBound level) buf (i * layout.SampleBytes))
        ((tallies, stats), Array.indexed results)
            ||> Array.fold (fun (tallies : Map<string, Tally>, (nPass, nGame, nRegrets, maxAbs, sumAbs)) (i, r) ->
                let tallies =
                    (tallies, r.Findings |> List.distinctBy _.Category)   // count each sample once per category
                        ||> List.fold (fun tallies f ->
                            let tally =
                                tallies
                                    |> Map.tryFind f.Category
                                    |> Option.defaultValue { Count = 0L; Examples = [] }
                            let examples =
                                if tally.Examples.Length < numExamples then
                                    tally.Examples @ [ firstIndex + int64 i, f.Detail ]
                                else tally.Examples
                            tallies |> Map.add f.Category { Count = tally.Count + 1L; Examples = examples })
                let stats =
                    (if r.IsPass then nPass + 1L else nPass),
                    (if r.HasGameScore then nGame + 1L else nGame),
                    nRegrets + int64 r.NumRegrets,
                    max maxAbs r.MaxAbsRegret,
                    sumAbs + r.SumAbsRegret
                tallies, stats)

    /// Counts the samples with findings among the first few, to choose between layouts.
    let private trial (layout : Layout) level (path : string) numSamples =
        let count = int (min 200L numSamples)
        use stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        stream.Seek(int64 Layout.headerBytes, SeekOrigin.Begin) |> ignore
        let buf = Array.zeroCreate<byte> (count * layout.SampleBytes)
        stream.ReadExactly(buf, 0, buf.Length)
        Array.init count (fun i -> Check.check layout (regretBound level) buf (i * layout.SampleBytes))
            |> Array.filter (fun r -> not r.Findings.IsEmpty)
            |> Array.length

    /// Compares a few samples as the trainer's own reader reads them with the validator's decoding.
    let private crossCheck (layout : Layout) (path : string) numSamples =
        [
            use store = AdvantageSampleStore.openRead path
            use stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
            for idx in List.distinct [ 0L; numSamples / 2L; numSamples - 1L ] do
                let sample = store[idx]
                let buf = Array.zeroCreate<byte> layout.SampleBytes
                stream.Seek(int64 Layout.headerBytes + idx * int64 layout.SampleBytes, SeekOrigin.Begin) |> ignore
                stream.ReadExactly(buf, 0, buf.Length)
                let bits = Array.init layout.NumInputs (Check.getBit buf 0)
                let regrets = Array.zeroCreate<float32> 52
                for j in 0 .. Layout.MaxRegrets - 1 do
                    let pos = layout.EncodingBytes + j * 5
                    if buf[pos] <> Byte.MaxValue && int buf[pos] < 52 then
                        regrets[int buf[pos]] <- BitConverter.ToSingle(buf, pos + 1)
                if sample.Encoding <> bits || sample.Regrets.ToArray() <> regrets then
                    $"sample {idx} reads differently through AdvantageSampleStore"
                if sample.Iteration <> store.Iteration then
                    $"sample {idx} has iteration {sample.Iteration}, its store {store.Iteration}"
        ]

    /// Validates one store, printing a report. Answers whether it is free of errors.
    let private validate (path : string) =
        let name = Path.GetFileName(path)
        printfn $"{name}"
        let errors = ResizeArray<string>()
        let warnings = ResizeArray<string>()
        let notes = ResizeArray<string>()

            // file name: payoff level and iteration
        let nameMatch = nameRegex.Match(name)
        let level, nameIterOpt =
            if nameMatch.Success then
                let level =
                    match nameMatch.Groups[1].Value with
                        | "Game" -> GameLevel
                        | "Deal" -> DealLevel
                        | _ ->
                            notes.Add("no payoff level in the name (before 2619a2e): deal level assumed")
                            DealLevel
                level, Some (Int32.Parse(nameMatch.Groups[2].Value))
            else
                warnings.Add("unrecognized file name: deal level assumed")
                DealLevel, None

            // header
        let length = FileInfo(path).Length
        let header =
            if length < int64 Layout.headerBytes then None
            else
                use stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
                let buf = Array.zeroCreate<byte> Layout.headerBytes
                stream.ReadExactly(buf, 0, buf.Length)
                Some (buf[0 .. 3], BitConverter.ToInt32(buf, 4))
        match header with
            | None -> errors.Add($"only {length} bytes: no header")
            | Some (magic, iteration) ->
                if magic <> "Hrts"B then
                    errors.Add($"bad magic bytes {BitConverter.ToString(magic)}")
                if iteration < 1 then
                    errors.Add($"iteration {iteration} in the header")
                match nameIterOpt with
                    | Some nameIter when nameIter <> iteration ->
                        errors.Add($"the header says iteration {iteration}, the name {nameIter}")
                    | _ -> ()

        let ok =
            if errors.Count > 0 then false
            else

                // choose the encoding's layout by size, and by trial if more than one fits (or none)
            let dataBytes = length - int64 Layout.headerBytes
            let fits (layout : Layout) = dataBytes % int64 layout.SampleBytes = 0L
            let layout =
                match Layout.all |> List.filter fits with
                    | [ layout ] -> layout
                    | candidates ->
                        let candidates = if candidates.IsEmpty then Layout.all else candidates
                        candidates
                            |> List.filter (fun l -> dataBytes >= int64 l.SampleBytes)
                            |> List.sortBy (fun l -> trial l level path (dataBytes / int64 l.SampleBytes))
                            |> List.tryHead
                            |> Option.defaultValue Layout.v2
            let numSamples = dataBytes / int64 layout.SampleBytes
            let trailing = dataBytes % int64 layout.SampleBytes
            if trailing <> 0L then
                errors.Add($"truncated: {trailing} bytes after the last whole sample of {layout.SampleBytes}")
            if level = GameLevel && layout.GameScoreWidth = 0 then
                errors.Add("game-level name, but the encoding has no game score")
            let iteration = header |> Option.map snd |> Option.defaultValue 0
            printfn $"   encoding: {layout.Name}"
            printfn $"""   payoff: {(match level with GameLevel -> "game" | DealLevel -> "deal")} level; iteration {iteration}"""

                // check every sample
            let tallies, (nPass, nGame, nRegrets, maxAbs, sumAbs) =
                use stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, 1 <<< 20)
                stream.Seek(int64 Layout.headerBytes, SeekOrigin.Begin) |> ignore
                (seq { 0L .. int64 samplesPerChunk .. numSamples - 1L })
                    |> Seq.fold (fun state first ->
                        let count = int (min (int64 samplesPerChunk) (numSamples - first))
                        checkChunk layout level stream first count state)
                        (Map.empty, (0L, 0L, 0L, 0f, 0.0))
            let percent n = if numSamples = 0L then 0.0 else 100.0 * float n / float numSamples
            printfn $"   {numSamples:N0} samples: %.1f{percent nPass}%% passing, %.1f{percent (numSamples - nPass)}%% playing"
            if layout.GameScoreWidth > 0 then
                printfn $"   game score other than 0-0-0-0 in %.1f{percent nGame}%% of samples"
            if nRegrets > 0L then
                printfn $"   regrets: %.2f{float nRegrets / float numSamples} per sample, mean |r| %.4f{sumAbs / float nRegrets}, max |r| %.4f{maxAbs}"

                // whole-store expectations
            if numSamples = 0L then
                warnings.Add("no samples")
            elif layout.GameScoreWidth > 0 && nGame = 0L then
                if level = GameLevel then
                    errors.Add("not game-aware: every game score is 0-0-0-0")
                else
                    notes.Add("every game score is 0-0-0-0: the traversal didn't see the game score")
            if level = DealLevel && numSamples >= 1000L && nRegrets > 0L && maxAbs <= 2f then
                warnings.Add($"deal-level name, but no regret exceeds 2 (max %.4f{maxAbs}): game-level payoffs?")

                // the trainer's reader must agree with the validator's
            if layout = Layout.v2 && layout.NumInputs = Hearts.Model.Model.inputSize
                && trailing = 0L && numSamples > 0L then
                for problem in crossCheck layout path numSamples do
                    errors.Add(problem)

            for KeyValue(category, tally) in tallies do
                errors.Add(
                    let examples =
                        tally.Examples
                            |> List.map (fun (idx, detail) -> $"\n         #{idx}: {detail}")
                            |> String.concat ""
                    $"{category}: {tally.Count:N0} samples{examples}")
            errors.Count = 0

        for note in notes do printfn $"   note: {note}"
        for warning in warnings do printfn $"   WARNING: {warning}"
        for error in errors do printfn $"   ERROR: {error}"
        if ok && errors.Count = 0 then printfn "   OK"
        printfn ""
        ok && errors.Count = 0

    /// The stores named by the given paths: files, or every store in a folder.
    let private findStores (paths : string[]) =
        paths
            |> Array.collect (fun path ->
                if Directory.Exists(path) then
                    Directory.GetFiles(path, "AdvantageSamples*.bin") |> Array.sort
                else [| path |])

    [<EntryPoint>]
    let main argv =
        Console.OutputEncoding <- Encoding.UTF8
        if argv.Length = 0 then
            printfn "usage: Hearts.ValidateSamples <store.bin | folder> ..."
            2
        else

            // the validator's own layout must describe the current encoding
        if Layout.v2.NumInputs <> Hearts.Model.Encoding.encodedLength then
            printfn $"WARNING: the current encoding has {Hearts.Model.Encoding.encodedLength} inputs, which no layout in Layout.fs describes"
        assert(Hearts.Model.Card.toIndex (Card.create Rank.Queen Suit.Spades) = 49)

        let stores = findStores argv
        let results = stores |> Array.map validate
        let numBad = results |> Array.filter not |> Array.length
        printfn $"{stores.Length} stores, {numBad} with errors"
        if numBad = 0 then 0 else 1
