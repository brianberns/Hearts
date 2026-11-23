namespace Hearts.Experiments

open System
open System.Diagnostics

open Hearts
open Hearts.Heuristic
open Hearts.Model

/// Tries to break TorchSharp by running tournaments on the GPU from many threads: checks for crashes, for
/// results that differ from a sequential run, and for memory that grows from round to round.
module Gpu =

    /// GPU memory in use, in MiB, according to nvidia-smi.
    let private gpuMemory () =
        let info = ProcessStartInfo("nvidia-smi", "--query-gpu=memory.used --format=csv,noheader,nounits", RedirectStandardOutput = true)
        use proc = Process.Start info
        let output = proc.StandardOutput.ReadToEnd()
        proc.WaitForExit()
        output.Trim()

    /// This process's private memory, in MiB.
    let private processMemory () =
        use proc = Process.GetCurrentProcess()
        proc.PrivateMemorySize64 / (1024L * 1024L)

    /// The given player, with each decision in its own dispose scope, so that every tensor it creates is
    /// disposed when the decision is made instead of waiting for the garbage collector.
    let private scoped (player : Player) =
        let act infoSet =
            use _ = TorchSharp.torch.NewDisposeScope()
            player.Act infoSet
        { Act = act }

    /// Runs the given number of rounds, each a parallel tournament of the given number of games, optionally
    /// with each decision in a dispose scope.
    let run modelPath numGames numRounds isScoped =
        printfn $"CUDA available: {TorchSharp.torch.cuda.is_available ()}, {Environment.ProcessorCount} cores"
        use model = Players.loadModelOn TorchSharp.torch.CUDA modelPath
        let player = Strategy.createPlayer model |> (if isScoped then scoped else id)
        printfn $"dispose scopes: {isScoped}"
        let tournament inParallel =
            let watch = Stopwatch.StartNew()
            let rate = Tournament.run 0 inParallel numGames Claude.player player
            rate, watch.Elapsed.TotalSeconds
        let report label (rate : float32) seconds =
            GC.Collect()
            GC.WaitForPendingFinalizers()
            printfn $"%-12s{label} rate %.4f{rate}  %6.1f{seconds} s  process %6d{processMemory ()} MiB  GPU %s{gpuMemory ()} MiB"
        printfn $"before:      process %6d{processMemory ()} MiB  GPU %s{gpuMemory ()} MiB"
        let baseRate, seconds = tournament false
        report "sequential" baseRate seconds
        for round in 1 .. numRounds do
            let rate, seconds = tournament true
            report $"parallel {round}" rate seconds
            if rate <> baseRate then
                printfn $"   DIFFERS from sequential ({rate} vs {baseRate})"
