/// Experiments with Hearts players.
///
///    bench <model> <games> [torch threads]
///        times the model, model at South vs three Killer, at 1, 2, 4, 8 and all cores' threads, and checks
///        that it plays the same in parallel as sequentially
///    phase0 <production model> <weakened model> <games>
///        the two models against each other and against Killer, by game wins under arena rules
///    gpu <model> <games> <rounds> [scoped]
///        rounds of tournaments on the GPU, checking for crashes, results that differ from round 1,
///        and memory growth; "scoped" puts each decision in a dispose scope
///    benchmark <model> <seed> <games>
///        the benchmarks for model training: Killer Hearts V8 vs three V7.1, and the model vs three V8
///    tune <seed> <games> [variation ...]
///        variations of Killer Hearts V8 (see Tune.variations), each vs three V8 and three V7.1
module Hearts.Experiments.Program

open System
open System.Text

[<EntryPoint>]
let main argv =
    Console.OutputEncoding <- Encoding.UTF8
    match argv with
        | [| "bench"; model; games |] -> Bench.run model (int games) None; 0
        | [| "bench"; model; games; threads |] -> Bench.run model (int games) (Some (int threads)); 0
        | [| "phase0"; production; weakened; games |] -> Phase0.run production weakened (int games); 0
        | [| "benchmark"; model; seed; games |] -> Benchmark.run model (int seed) (int games); 0
        | args when args.Length >= 3 && args[0] = "tune" -> Tune.run (int args[1]) (int args[2]) (List.ofArray args[3..]); 0
        | [| "gpu"; model; games; rounds |] -> Gpu.run model (int games) (int rounds) false; 0
        | [| "gpu"; model; games; rounds; "scoped" |] -> Gpu.run model (int games) (int rounds) true; 0
        | _ ->
            eprintfn "usage: Hearts.Experiments bench <model> <games> [torch threads] | phase0 <production model> <weakened model> <games> | gpu <model> <games> <rounds> [scoped] | benchmark <model> <seed> <games> | tune <seed> <games> [variation ...]"
            1
