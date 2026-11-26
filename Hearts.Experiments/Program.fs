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
///        the benchmark for model training: the model vs three Killer Hearts V8
///    wdata <model> <seed> <games> <output.csv>
///        whole games with the model in every seat, writing each deal's starting scores, pass direction and
///        the game's winner, the data for W (each seat's chance of winning the game from a deal's start)
///    wfit <data.csv> <output.pt> <epochs>
///        fits W to wdata's data from the four game scores, and reports its loss, calibration and some values
///    tune <seed> <games> [variation ...]
///        variations of Killer Hearts V8 (see Tune.variations), each vs three V8
///    duplicate <seed> <games>
///        how much duplicate deals narrow a win rate's error bar: Gemini and Killer Hearts V8, each vs three
///        of the other
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
        | [| "wdata"; model; seed; games; output |] -> WData.run model (int seed) (int games) output; 0
        | [| "wfit"; data; output; epochs |] -> WFit.run data output (int epochs); 0
        | [| "benchmark"; model; seed; games |] -> Benchmark.run model (int seed) (int games); 0
        | args when args.Length >= 3 && args[0] = "tune" -> Tune.run (int args[1]) (int args[2]) (List.ofArray args[3..]); 0
        | [| "duplicate"; seed; games |] -> Duplicate.run (int seed) (int games); 0
        | [| "gpu"; model; games; rounds |] -> Gpu.run model (int games) (int rounds) false; 0
        | [| "gpu"; model; games; rounds; "scoped" |] -> Gpu.run model (int games) (int rounds) true; 0
        | _ ->
            eprintfn "usage: Hearts.Experiments bench <model> <games> [torch threads] | phase0 <production model> <weakened model> <games> | gpu <model> <games> <rounds> [scoped] | benchmark <model> <seed> <games> | wdata <model> <seed> <games> <output.csv> | wfit <data.csv> <output.pt> <epochs> | tune <seed> <games> [variation ...] | duplicate <seed> <games>"
            1
