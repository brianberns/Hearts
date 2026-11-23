/// Experiments with Hearts players.
///
///    bench <model> <games> [torch threads]
///        times the model, model at South vs three Claude, at 1, 2, 4, 8 and all cores' threads, and checks
///        that it plays the same in parallel as sequentially
///    phase0 <production model> <weakened model> <games>
///        the two models against each other and against Claude, by game wins under arena rules
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
        | _ ->
            eprintfn "usage: Hearts.Experiments bench <model> <games> [torch threads] | phase0 <production model> <weakened model> <games>"
            1
