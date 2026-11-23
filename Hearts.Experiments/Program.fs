/// Experiments with Hearts players.
///
///    bench <model> <games> [torch threads]
///        times the model, model at South vs three Claude, at 1, 2, 4, 8 and all cores' threads, and checks
///        that it plays the same in parallel as sequentially
///    phase0 <production model> <weakened model> <games>
///        the two models against each other and against Claude, by game wins under arena rules
///    gpu <model> <games> <rounds> [scoped]
///        rounds of tournaments on the GPU, checking for crashes, results that differ from round 1,
///        and memory growth; "scoped" puts each decision in a dispose scope
///    determinism <model> <games>
///        the deterministic model player against the sampling one, head to head both ways and each vs
///        three Claude
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
        | [| "determinism"; model; games |] -> Determinism.run model (int games); 0
        | [| "gpu"; model; games; rounds |] -> Gpu.run model (int games) (int rounds) false; 0
        | [| "gpu"; model; games; rounds; "scoped" |] -> Gpu.run model (int games) (int rounds) true; 0
        | _ ->
            eprintfn "usage: Hearts.Experiments bench <model> <games> [torch threads] | phase0 <production model> <weakened model> <games> | gpu <model> <games> <rounds> [scoped] | determinism <model> <games>"
            1
