namespace Hearts.PlayKiller

open System
open System.Diagnostics
open System.Runtime

open Hearts
open Hearts.Heuristic
open Hearts.Model

module Program =

    printfn $"Server garbage collection: {GCSettings.IsServerGC}"

    let numDeals = 20_000

    do
        use model =
            let model =
                new AdvantageModel(
                    hiddenSize = Encoding.encodedLength * 3,
                    numHiddenLayers = 9,
                    dropoutRate = 0.3,
                    device = TorchSharp.torch.CUDA)
            model.load("AdvantageModel.pt") |> ignore
            model.eval()
            model
        let player = Strategy.createPlayer true model

        let stopwatch = Stopwatch.StartNew()
        let payoff =
            Tournament.run 0 true numDeals Claude.player player
        printfn $"Payoff: {payoff}"
        printfn $"Elapsed time: {stopwatch.Elapsed}"

    Console.ReadLine() |> ignore
