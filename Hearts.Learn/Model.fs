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
        /// Information available to the player at the time
        /// of the event (hand + deal).
        InfoSet : Encoding

        /// Observed regrets.
        Regrets : Vector<float32>

        /// Weight of this sample, as determined by 1-based
        /// iteration number.
        Weight : float32
    }

module AdvantageSample =

    /// Creates an advantage sample.
    let create hand deal regrets iteration =
        assert(Vector.length regrets = Network.outputSize)
        assert(iteration > 0)
        assert(iteration <= settings.NumIterations)
        {
            InfoSet = Encoding.encode hand deal
            Regrets = regrets
            Weight =
                iteration
                    |> float32
                    |> sqrt
        }

module AdvantageModel =

    type private SubBatch =
        {
            Inputs : byte array2d
            Targets : float32 array2d
            Iterations : float32 array2d
        }

    module private SubBatch =

        let create inputs targets iters =
            {
                Inputs = array2D inputs
                Targets = array2D targets
                Iterations = array2D iters
            }

    type private Batch = SubBatch[]

    /// Breaks the given samples into batches.
    let private createBatches samples : Batch[] =
        samples
            |> Seq.toArray
            |> Array.randomShuffle
            |> Array.chunkBySize     // e.g. sub-batches of 10,000 rows each
                settings.AdvantageSubBatchSize
            |> Array.chunkBySize (   // e.g. each batch contains 500,000 / 10,000 = 50 sub-batches
                settings.AdvantageBatchSize
                    / settings.AdvantageSubBatchSize)
            |> Array.map (
                Array.map (fun samples ->
                    let inputs, targets, iters =
                        samples
                            |> Array.map (fun sample ->
                                sample.InfoSet,
                                sample.Regrets,
                                Seq.singleton sample.Weight)
                            |> Array.unzip3
                    SubBatch.create inputs targets iters))

    /// Trains the given model on the given batch of data
    /// using gradient accumulation.
    /// https://chat.deepseek.com/a/chat/s/2f688262-70d6-4fb9-a05e-c230fa871f83
    let private trainBatch
        model
        (batch : Batch)
        (criterion : Loss<Tensor, Tensor, Tensor>)
        (optimizer : Optimizer) =

        let loss =
            Array.last [|
                for subbatch in batch do

                        // move to GPU
                    use inputs =
                        tensor(
                            subbatch.Inputs,
                            device = settings.Device,
                            dtype = ScalarType.Float32)
                    use targets =
                        tensor(
                            subbatch.Targets,
                            device = settings.Device)
                    use iters =
                        tensor(
                            subbatch.Iterations,
                            device = settings.Device)

                        // forward pass
                    use loss =
                        use outputs = inputs --> model
                        use outputs' = iters * outputs   // favor later iterations
                        use targets' = iters * targets
                        criterion.forward(outputs', targets')

                        // backward pass
                    use scaledLoss =
                        let scale =
                            float32 (subbatch.Inputs.GetLength(0))
                                / float32 settings.AdvantageBatchSize
                        loss * scale
                    scaledLoss.backward()

                    loss.item<float32>()
            |]

            // optimize
        use _ = optimizer.step()
        optimizer.zero_grad()

        loss

    /// Trains the given model using the given samples.
    let train iter samples (model : AdvantageModel) =

            // prepare training data
        let batches = createBatches samples

            // train model
        use optimizer =
            Adam(
                model.parameters(),
                settings.LearningRate)
        use criterion = MSELoss()
        model.train()
        for epoch = 1 to settings.NumAdvantageTrainEpochs do
            let loss =
                Array.last [|
                    for batch in batches do
                        trainBatch model batch criterion optimizer
                |]
            settings.Writer.add_scalar(
                $"advantage loss/iter%03d{iter}",
                loss, epoch)
        model.eval()
