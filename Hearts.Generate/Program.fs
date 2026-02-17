namespace Hearts.Generate

open System
open System.Diagnostics
open System.IO
open System.Runtime
open System.Text

open Hearts
open Hearts.Learn
open Hearts.Model

module Program =

    /// Generates training data for the given iteration.
    let private generateSamples settings iteration state =

        /// TensorBoard logging.
        let log value step =
            settings.Writer.add_scalar(
                $"advantage samples/iter%03d{iteration}",
                value, step)

            // start TensorBoard y-axis at 0
        log 0f 0

            // divide deals for this iteration into batches,
            // including possible runt batch at the end
        let batchSizes =
            Seq.replicate settings.NumDealsPerIteration ()
                |> Seq.chunkBySize settings.DealBatchSize
                |> Seq.map _.Length

            // generate samples for each batch
        Array.sum [|
            for iBatch, numDeals in Seq.indexed batchSizes do
                assert(numDeals <= settings.DealBatchSize)

                    // generate samples
                let samples =
                    OpenDeal.playDeals (Random()) true numDeals
                        (fun deal ->
                            let rng = Random()   // each thread has its own RNG
                            Traverse.traverse settings iteration deal rng)
                        |> Inference.complete
                            settings.InferenceBatchSize
                            state.ModelOpt

                    // save samples
                AdvantageSampleStore.writeSamples
                    samples state.SampleStore
                log
                    (float32 samples.Length / float32 numDeals)    // average number of generated samples per deal in this batch
                    (iBatch * settings.DealBatchSize + numDeals)   // total number of deals so far

                samples.Length
        |]

    /// Parses command line arguments.
    let private parse (argv : string[]) =
        match argv.Length with
            | 0 -> None, 1
            | 1 ->
                let modelPath = argv[0]
                let iter =
                    let fileName = Path.GetFileNameWithoutExtension(modelPath)
                    fileName[fileName.Length - 3 ..]   // e.g. "AdvantageModel001.pt"
                        |> Int32.Parse
                Some modelPath, iter
            | _ -> failwith $"Invalid arguments: {argv}"

    /// Generates samples for the given iteration using the given model.
    let private run modelPathOpt iteration =

            // get settings
        let settings =
            let writer = TensorBoard.createWriter ()
            Settings.create writer
        Settings.write settings
        if settings.Verbose then
            printfn $"Server garbage collection: {GCSettings.IsServerGC}"
            printfn $"Settings: {settings}"
            printfn $"Model input size: {Model.inputSize}"
            printfn $"Model output size: {Model.outputSize}"

            // initialize model, if specified
        let modelOpt =
            modelPathOpt
                |> Option.map (fun modelPath ->
                    let model =
                        new AdvantageModel(
                            settings.HiddenSize,
                            settings.NumHiddenLayers,
                            0.0,   // dropout not used during inference
                            settings.Device)
                    model.load(modelPath : string) |> ignore
                    model.eval()
                    model)

            // initialize state
        use state =
            let unique =
                let timespan = DateTime.Now - DateTime.Today
                int timespan.TotalSeconds
            Path.Combine(
                settings.ModelDirPath,
                $"AdvantageSamples%03d{iteration}.%05d{unique}.bin")
                |> AdvantageSampleStore.create iteration
                |> AdvantageState.create modelOpt

            // generate samples
        let stopwatch = Stopwatch.StartNew()
        let numSamples = generateSamples settings iteration state
        if settings.Verbose then
            printfn $"\n{numSamples} samples generated in {stopwatch.Elapsed}"

    [<EntryPoint>]
    let main argv =
        Console.OutputEncoding <- Encoding.UTF8
        let modelPathOpt, iter = parse argv
        run modelPathOpt iter
        0
