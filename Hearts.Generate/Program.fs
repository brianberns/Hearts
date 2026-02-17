namespace Hearts.Generate

open System
open System.Diagnostics
open System.IO
open System.Runtime
open System.Text

open Hearts.Learn
open Hearts.Model

module Program =

    let run modelOpt iteration =

        let settings =
            let writer = TensorBoard.createWriter ()
            Settings.create writer
        Settings.write settings
        if settings.Verbose then
            printfn $"Server garbage collection: {GCSettings.IsServerGC}"

        let state =
            Path.Combine(
                settings.ModelDirPath,
                $"AdvantageSamples.{iteration}.{DateTime.Now.Ticks}")
                |> AdvantageSampleStore.create iteration
                |> AdvantageState.create modelOpt

        let stopwatch = Stopwatch.StartNew()
        let numSamples = Trainer.generateSamples settings iteration state
        if settings.Verbose then
            printfn $"\n{numSamples} samples generated in {stopwatch.Elapsed}"

    [<EntryPoint>]
    let main argv =
        Console.OutputEncoding <- Encoding.UTF8

        let modelOpt, iter =
            if argv.Length = 0 then
                None, 0
            else
                let modelPath = argv[0]
                let iter =
                    let fileName = Path.GetFileNameWithoutExtension(modelPath)
                    fileName[fileName.Length - 3 ..]   // e.g. "AdvantageModel001.pt"
                        |> Int32.Parse
                let model =
                    new AdvantageModel(
                        hiddenSize = Encoding.encodedLength * 3,
                        numHiddenLayers = 9,
                        dropoutRate = 0.3,
                        device = TorchSharp.torch.CUDA)
                model.load(argv[0]) |> ignore
                Some model, iter

        run modelOpt iter

        0
