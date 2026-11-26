namespace Hearts.Generate

open System
open System.Diagnostics
open System.IO
open System.Runtime
open System.Text

open PlayingCards

open Hearts
open Hearts.Learn
open Hearts.Model

module Program =

    /// Creates a deal for the given index.
    let private createDeal rng iDeal =
        let dealer =
            enum<Seat> (iDeal % Seat.numSeats)
        let dir =
            enum<ExchangeDirection>
                (iDeal % ExchangeDirection.numDirections)
        let deck = Deck.shuffle rng
        OpenDeal.fromDeck dealer dir deck

    /// Generates an infinite sequence of game states with rotating
    /// dealers and exchange directions. Each game state has a
    /// current deal, so this is best thought of as a sequence of
    /// deals within and across games.
    let private generate rules rng =
        Seq.initInfinite (fun _ ->
            GameScore.generate rules rng)   // infinite sequence of game scores
            |> Seq.collect (
                Seq.mapi (fun iDeal score ->
                    let deal = createDeal rng iDeal
                    Game.create deal score))

    /// Plays the current deal in the given games in parallel.
    let private playDeals settings state games =
        games
            |> Array.Parallel.map (fun (game : Game) ->
                let rng = Random()   // give each deal its own RNG
                Traverse.traverse settings state rng game.Deal)

    /// Generates training data for the given iteration.
    let private generateSamples settings state =

        /// TensorBoard logging.
        let log value step =
            settings.Writer.add_scalar(
                $"advantage samples/iter%03d{state.SampleStore.Iteration}",
                value, step)

            // start TensorBoard y-axis at 0
        log 0f 0

            // divide deals for this iteration into batches,
            // including possible runt batch at the end
        let dealBatches =
            let rng = Random()
            generate settings.Rules rng
                |> Seq.take settings.NumDealsPerIteration
                |> Seq.chunkBySize settings.DealBatchSize

            // generate samples for each batch
        Array.sum [|
            for iBatch, games in Seq.indexed dealBatches do

                    // generate samples
                let samples =
                    playDeals settings state games
                        |> Inference.complete
                            settings.InferenceBatchSize
                            state.AdvantageModelOpt

                    // save samples
                AdvantageSampleStore.appendSamples
                    samples state.SampleStore
                let nDeals = games.Length                        // each game state has one current deal
                log
                    (float32 samples.Length / float32 nDeals)    // average number of generated samples per deal in this batch
                    (iBatch * settings.DealBatchSize + nDeals)   // total number of deals so far

                samples.Length
        |]

    /// Parses command line arguments.
    let private parse (argv : string[]) =
        match argv.Length with
            | 0 -> None, 0
            | 1 ->
                let modelPath = argv[0]
                let iter =
                    let chunks =
                        Path.GetFileNameWithoutExtension(modelPath)   // e.g. "AdvantageModel-i001.pt"
                            .Split('-')
                    Int32.Parse(chunks[1][1 ..])
                Some modelPath, iter
            | _ -> failwith $"Invalid arguments: {argv}"

    /// Generates samples for the given iteration using the given
    /// advantage model.
    let private run advModelPathOpt iteration =

            // get settings
        let settings =
            let writer = TensorBoard.createWriter ()
            Settings.create writer
        Settings.write settings
        if settings.Verbose then
            printfn "Settings:"
            printfn $"   Server garbage collection: {GCSettings.IsServerGC}"
            printfn $"   Iteration: {iteration}"
            printfn $"   # deals to generate: {settings.NumDealsPerIteration}"
            printfn $"   Deal batch size: {settings.DealBatchSize}"
            printfn $"   Inference batch size: {settings.InferenceBatchSize}"
            printfn $"   Sample branch rate: {settings.SampleBranchRate}"
            printfn $"   Game level payoff: {settings.GameLevelPayoff}"
            printfn $"   Hidden size: {settings.HiddenSize}"
            printfn $"   # hidden layers: {settings.NumHiddenLayers}"
            printfn $"   Device: {settings.Device}"
            printfn $"   Model directory: {settings.ModelDirPath}"
            printfn $"   Model input size: {Model.inputSize}"
            printfn $"   Model output size: {Model.outputSize}"

            // initialize win probability model?
        let winProbModelOpt =
            if settings.GameLevelPayoff then
                Some (WinProbability.load settings)
            else None

            // initialize advantage model, if specified
        let advModelOpt =
            advModelPathOpt
                |> Option.map (fun modelPath ->
                    let model =
                        new AdvantageModel(
                            settings.HiddenSize,
                            settings.NumHiddenLayers,
                            0.0,   // dropout not used during inference
                            settings.Device)
                    if settings.Verbose then
                        printfn $"Loading model from {modelPath}"
                    model.load(modelPath : string) |> ignore
                    model.eval()
                    model)

            // initialize state
        use state =
            let unique =
                let timespan = DateTime.Now - DateTime.Today
                int timespan.TotalSeconds
            let path =
                let level =
                    if settings.GameLevelPayoff then "Game"
                    else "Deal"
                Path.Combine(
                    settings.ModelDirPath,
                    $"AdvantageSamples-{level}-i%03d{iteration}-%05d{unique}.bin")
            if settings.Verbose then
                printfn $"Creating sample store: {path}"
            path
                |> AdvantageSampleStore.create iteration
                |> AdvantageState.create winProbModelOpt advModelOpt

            // generate samples
        let stopwatch = Stopwatch.StartNew()
        let numSamples = generateSamples settings state
        if settings.Verbose then
            printfn $"{numSamples} samples generated in {stopwatch.Elapsed}"

    /// Generates samples for the next iteration.
    [<EntryPoint>]
    let main argv =
        Console.OutputEncoding <- Encoding.UTF8
        let advModelPathOpt, iter = parse argv
        run advModelPathOpt (iter + 1)
        0
