namespace Hearts.PlayKiller

open System
open System.Runtime
open System.Text

open Hearts.Learn
open Hearts.Model
open Hearts.Train

module Program =

    let parse (argv : _[]) =
        if argv.Length = 0 then
            "AdvantageModel.pt"
        else
            argv[0]

    let run path =

        let settings =
            let writer = TensorBoard.createWriter ()
            Settings.create writer
        Settings.write settings

        printfn $"Server garbage collection: {GCSettings.IsServerGC}"

        use model =
            new AdvantageModel(
                settings.HiddenSize,
                settings.NumHiddenLayers,
                0.0,
                TorchSharp.torch.CPU)   // always run on CPU
        model.load(path : string) |> ignore
        model.eval()

        let player = Strategy.createPlayer model

        let payoffMap = Killer.run player
        for (KeyValue(seat, payoff)) in payoffMap do
            printfn "%A: %g" seat payoff

    [<EntryPoint>]
    let main argv =
        Console.OutputEncoding <- Encoding.UTF8
        parse argv |> run
        0
