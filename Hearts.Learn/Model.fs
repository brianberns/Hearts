namespace Hearts.Learn

open TorchSharp
open type torch
open type torch.nn
open type torch.optim
open FSharp.Core.Operators   // reclaim "float32" and other F# operators

open MathNet.Numerics.LinearAlgebra

open Hearts.Model

/// An observed advantage event.
type AdvantageSample =
    {
        /// Encoded info set.
        Encoding : Encoding

        /// Observed regrets.
        Regrets : Vector<float32>

        /// Weight of this sample, as determined by 1-based
        /// iteration number. Later iterations have more weight.
        Weight : float32
    }

module AdvantageSample =

    /// Creates an advantage sample.
    let create infoSet regrets iteration =
        assert(Vector.length regrets = Model.outputSize)
        assert(iteration >= 1)
        {
            Encoding = Encoding.encode infoSet
            Regrets = regrets
            Weight = float32 iteration |> sqrt
        }

module AdvantageModel =

    /// A chunk of training data that fits on the GPU.
    type private SubBatch = AdvantageSample[]

    module private SubBatch =

        /// Extracts inputs from a sub-batch.
        let private toInputs subbatch =
            subbatch
                |> Array.map _.Encoding
                |> array2D

        /// Extracts targets from a sub-batch.
        let private toTargets subbatch =
            subbatch
                |> Array.map _.Regrets
                |> array2D

        /// Extracts weights from a sub-batch.
        let private toWeights subbatch =
            subbatch
                |> Array.map (_.Weight >> Array.singleton)
                |> array2D

        /// Extracts 2D arrays from a sub-batch.
        let to2dArrays (subbatch : SubBatch) =
            let arrays =
                Array.Parallel.map (fun f -> f subbatch)
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

    /// A logical batch of training data, although it
    /// might be too large to fit on the GPU.
    type private Batch = SubBatch[]

    /// Breaks the given samples into batches.
    let private createBatches batchSize subBatchSize samples : Batch[] =
        samples
            |> Seq.toArray
            |> Array.randomShuffle
            |> Array.chunkBySize subBatchSize                 // e.g. sub-batches of 10,000 rows each
            |> Array.chunkBySize (batchSize / subBatchSize)   // e.g. each batch contains 500,000 / 10,000 = 50 sub-batches

    /// Trains the given model on the given sub-batch of
    /// data.
    let private trainSubBatch settings model subbatch
        (criterion : Loss<Tensor, Tensor, Tensor>) =

            // move to GPU
        let inputs2d, targets2d, weights2d =
            SubBatch.to2dArrays subbatch
        use inputs = tensor(inputs2d, device = settings.Device)
        use targets = tensor(targets2d, device = settings.Device)
        use weights = tensor(weights2d, device = settings.Device)

            // forward pass
        use loss =

                // compute loss for this sub-batch
            use rawLoss =
                use outputs = inputs --> model
                use outputs' = weights * outputs
                use targets' = weights * targets
                criterion.forward(outputs', targets')

                // scale loss to batch size
            let scale =
                float32 (inputs2d.GetLength(0))
                    / float32 settings.TrainingBatchSize
            rawLoss * scale

            // backward pass
        loss.backward()

        loss.item<float32>()

    /// Trains the given model on the given batch of data
    /// using gradient accumulation.
    /// https://chat.deepseek.com/a/chat/s/2f688262-70d6-4fb9-a05e-c230fa871f83
    let private trainBatch settings model (batch : Batch) criterion
        (optimizer : Optimizer) =

            // clear gradients
        optimizer.zero_grad()

            // train sub-batches
        let loss =
            Array.last [|
                for subbatch in batch do
                    trainSubBatch settings model subbatch criterion
            |]

            // optimize
        use _ = optimizer.step()

        loss

    /// Trains the given model using the given samples.
    let train iter samples (model : AdvantageModel) =

            // prepare training data
        let batches =
            createBatches
                settings.TrainingBatchSize
                settings.TrainingSubBatchSize
                samples

            // train model
        use optimizer =
            Adam(
                model.parameters(),
                settings.LearningRate)
        use criterion = MSELoss()
        model.train()
        for epoch = 1 to settings.NumTrainingEpochs do
            let loss =
                Array.last [|
                    for batch in batches do
                        trainBatch
                            settings model batch criterion optimizer
                |]
            settings.Writer.add_scalar(
                $"advantage loss/iter%03d{iter}",
                loss, epoch)
        model.eval()