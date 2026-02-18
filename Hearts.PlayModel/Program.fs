namespace Hearts.PlayModel

open System
open System.IO
open System.Runtime
open System.Text

open Microsoft.Extensions.FileSystemGlobbing

open Hearts
open Hearts.Heuristic
open Hearts.Learn
open Hearts.Model
open Hearts.Train

module Program =

    let parse (argv : _[]) =
        if argv.Length = 0 then
            seq { "AdvantageModel.pt" }
        else
            let matcher = Matcher()
            for arg in argv do
                matcher.AddInclude(arg) |> ignore
            matcher.GetResultsInFullPath(".")

    let run paths =

        let settings =
            let writer = TensorBoard.createWriter ()
            Settings.create writer
        Settings.write settings

        printfn $"Server garbage collection: {GCSettings.IsServerGC}"

        let numDeals = 20_000

        use model =
            new AdvantageModel(
                settings.HiddenSize,
                settings.NumHiddenLayers,
                0.0,
                settings.Device)

        for path in paths do
            model.load(path : string) |> ignore
            model.eval()
            let player = Strategy.createPlayer model
            let payoff =
                Tournament.run 0 true numDeals Claude.player player
            printfn $"{Path.GetFileName(path)}: {payoff}"

    [<EntryPoint>]
    let main argv =
        Console.OutputEncoding <- Encoding.UTF8
        parse argv |> run
        0
