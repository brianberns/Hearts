namespace Hearts.Train

open System
open System.IO
open System.Diagnostics

open TorchSharp
open type torch
open type torch.nn
open type torch.optim
open FSharp.Core.Operators   // reclaim "float32" and other F# operators

open Hearts.Learn
open Hearts.Model

module AdvantageModel =

    /// A chunk of training data that fits on the GPU and in
    /// memory.
    type private SubBatch = AdvantageSample[]

    module private SubBatch =

        /// Extracts inputs from a sub-batch.
        let private toInputs (samples : SubBatch) =
            samples
                |> Array.map (
                    _.Encoding
                        >> Encoding.toFloat32Array)
                |> array2D

        /// Extracts targets from a sub-batch.
        let private toTargets (samples : SubBatch) =
            samples
                |> Array.map _.Regrets
                |> array2D

        /// Extracts weights from a sub-batch.
        let private toWeights (samples : SubBatch) =
            samples
                |> Array.map (
                    _.Iteration
                        >> float32
                        >> sqrt   // MSE loss will square this, resulting in linear weight
                        >> Array.singleton)
                |> array2D

        /// Extracts 2D arrays from a sub-batch.
        let to2dArrays samples =
            let arrays =
                Array.Parallel.map (fun f -> f samples)
                    [| toInputs; toTargets; toWeights |]
            let inputs = arrays[0]
            let targets = arrays[1]
            let weights = arrays[2]
            assert(
                [
                    inputs.GetLength(0)
                    targets.GetLength(0)
                    weights.GetLength(0)
                ]
                    |> Seq.distinct
                    |> Seq.length = 1)
            assert(weights.GetLength(1) = 1)
            inputs, targets, weights

    /// A logical batch of training data that might be too
    /// large to fit on the GPU or in memory.
    type private Batch = seq<SubBatch>

    /// Indexes every sample in the given stores, optimized for
    /// space.
    let private indexSamples (sampleStores : AdvantageSampleStoreGroup) =
        [|
            let storeCount = sampleStores.Count
            assert(storeCount <= int32 Int16.MaxValue)

            for iStore = 0s to int16 storeCount - 1s do

                let sampleCount = sampleStores[int iStore].Count
                assert(sampleCount <= int64 Int32.MaxValue)

                for iSample = 0 to int sampleCount - 1 do
                    struct (iStore, iSample)
        |]

    /// Breaks the given samples into randomized batches.
    let private createBatches
        batchSize subBatchSize (indexPairs : _[])
        (sampleStores : AdvantageSampleStoreGroup) =

            // randomize index pairs into batches and sub-batches
        let indexBatches =
            [| 0 .. indexPairs.Length - 1 |]
                |> Array.randomShuffle
                |> Seq.chunkBySize subBatchSize                 // e.g. sub-batches of 10,000 rows each
                |> Seq.chunkBySize (batchSize / subBatchSize)   // e.g. each batch contains 500,000 / 10,000 = 50 sub-batches

            // stream samples
        seq {
            for indexBatch in indexBatches do
                let stopwatch = Stopwatch.StartNew()
                let batch : Batch =
                    seq {
                        for indexSubBatch in indexBatch do
                            indexSubBatch
                                |> Array.Parallel.map (fun iPair ->
                                    let struct (iStore, iSample) = indexPairs[iPair]
                                    sampleStores[int iStore][int64 iSample])
                    }
                batch, stopwatch
        }

    /// Mean and total weight of the given (value, weight) pairs.
    let private weightedMean pairs =
        let totalWeight = Array.sumBy snd pairs
        let total =
            Array.sumBy (fun (value : float32, weight : int) ->
                value * float32 weight)
                pairs
        (total / float32 totalWeight), totalWeight

    /// Computes loss for the given samples.
    let private getLoss device model samples
        (criterion : Loss<Tensor, Tensor, Tensor>) =

            // move to GPU
        let inputs2d, targets2d, weights2d =
            SubBatch.to2dArrays samples
        use inputs = tensor(inputs2d, device = device)
        use targets = tensor(targets2d, device = device)
        use weights = tensor(weights2d, device = device)

            // compute loss
        use outputs = inputs --> model
        use outputs' = weights * outputs
        use targets' = weights * targets
        criterion.forward(outputs', targets')

    /// Trains the given model on the given sub-batch of data,
    /// and answers its mean loss.
    let private trainSubBatch
        settings model samples criterion =

            // compute loss for this sub-batch
        use rawLoss = getLoss settings.Device model samples criterion

            // scale loss to batch size
        let scale =
            float32 samples.Length
                / float32 settings.TrainingBatchSize
        use scaledLoss = rawLoss * scale

            // backward pass on scaled loss, so accumulated gradients are the batch's
        scaledLoss.backward()

        rawLoss.item<float32>()

    /// Trains the given model on the given batch of data
    /// using gradient accumulation.
    /// https://chat.deepseek.com/a/chat/s/2f688262-70d6-4fb9-a05e-c230fa871f83
    let private trainBatch settings model (batch : Batch) criterion
        (optimizer : Optimizer) =

            // clear gradients
        optimizer.zero_grad()

            // train sub-batches
        let loss, nSamples =
            weightedMean [|
                for samples in batch do
                    trainSubBatch settings model samples criterion,
                    samples.Length
            |]

            // optimize
        use _ = optimizer.step()

        loss, nSamples

    /// Computes mean loss on the given samples, with the model in
    /// evaluation mode.
    let private getHoldoutLoss
        settings (model : AdvantageModel) samples criterion =

        model.eval()   // no dropout
        use _ = torch.no_grad()

        let loss, _ =
            let subBatches =
                Array.chunkBySize settings.TrainingSubBatchSize samples
            weightedMean [|
                for subBatch in subBatches do
                    use loss =
                        getLoss settings.Device model subBatch criterion
                    loss.item<float32>(), subBatch.Length
            |]

        model.train()   // back to training
        loss

    /// Trains the given model using the given samples.
    let train
        settings
        (sampleStores : AdvantageSampleStoreGroup)
        (model : AdvantageModel) =

            // index samples
        let holdoutPairs, trainingPairs =
            let indexPairs =
                indexSamples sampleStores
                    |> Array.randomShuffle
            Array.splitAt (indexPairs.Length / 100) indexPairs   // hold out 1% of samples

            // prepare holdout samples
        let holdoutSamples =
            holdoutPairs
                |> Array.Parallel.map (fun struct (iStore, iSample) ->
                    sampleStores[int iStore][int64 iSample])

            // train model
        use optimizer =
            Adam(
                model.parameters(),
                settings.LearningRate)
        use criterion = MSELoss()
        model.train()
        for epoch = 1 to settings.NumTrainingEpochs do

                // reshuffle samples for each epoch
            let batches =
                createBatches
                    settings.TrainingBatchSize
                    settings.TrainingSubBatchSize
                    trainingPairs
                    sampleStores

                // train epoch
            let loss, _ =
                weightedMean [|
                    for iBatch, (batch, stopwatch) in Seq.indexed batches do
                        trainBatch
                            settings model batch criterion optimizer
                        let seconds =
                            float32 stopwatch.ElapsedMilliseconds / 1000f
                        settings.Writer.add_scalar(
                            $"advantage training time/iter%03d{sampleStores.Iteration}/epoch%03d{epoch}",
                            seconds, iBatch)
                |]
            settings.Writer.add_scalar(
                $"advantage loss/iter%03d{sampleStores.Iteration}",
                loss, epoch)

                // evaluate holdout samples
            let loss =
                getHoldoutLoss settings model holdoutSamples criterion
            settings.Writer.add_scalar(
                $"holdout loss/iter%03d{sampleStores.Iteration}",
                loss, epoch)

                // save model
            let path =
                let level =
                    if settings.GameLevelPayoff then "Game"
                    else "Deal"
                Path.Combine(
                    settings.ModelDirPath,
                    $"AdvantageModel-{level}-i%03d{sampleStores.Iteration}-e%03d{epoch}.pt")
            model.save(path) |> ignore

        model.eval()
