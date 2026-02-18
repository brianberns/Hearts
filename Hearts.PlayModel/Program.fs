namespace Hearts.PlayModel

open System
open System.Diagnostics
open System.Runtime
open System.Text

open Hearts
open Hearts.Heuristic
open Hearts.Learn
open Hearts.Model
open Hearts.Train

module Program =

    let run path =

        let settings =
            let writer = TensorBoard.createWriter ()
            Settings.create writer
        Settings.write settings

        printfn $"Server garbage collection: {GCSettings.IsServerGC}"

        let numDeals = 20_000

        use model =
            let model =
                new AdvantageModel(
                    settings.HiddenSize,
                    settings.NumHiddenLayers,
                    0.0,
                    settings.Device)
            model.load(path : string) |> ignore
            model.eval()
            model
        let player = Strategy.createPlayer model

        let stopwatch = Stopwatch.StartNew()
        let payoff =
            Tournament.run 0 true numDeals Claude.player player
        printfn $"Payoff: {payoff}"
        printfn $"Elapsed time: {stopwatch.Elapsed}"


    [<EntryPoint>]
    let main argv =
        Console.OutputEncoding <- Encoding.UTF8
        let path =
            if argv.Length = 0 then "AdvantageModel.pt"
            else argv[0]
        run path
        0
