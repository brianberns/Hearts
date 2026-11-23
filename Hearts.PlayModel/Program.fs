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

        let numGames = 2000

        use model =
            new AdvantageModel(
                settings.HiddenSize,
                settings.NumHiddenLayers,
                0.0,
                TorchSharp.torch.CPU)   // always run on CPU

        for path in paths do
            model.load(path : string) |> ignore
            model.eval()
            let winRate =
                Tournament.runSeeded
                    0
                    numGames
                    (fun _ -> Claude.player)
                    (Strategy.createPlayerSeeded model)
            printfn $"{Path.GetFileName(path)}: {winRate}"

    [<EntryPoint>]
    let main argv =
        Console.OutputEncoding <- Encoding.UTF8
        parse argv |> run
        0
