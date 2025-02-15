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

    /// Breaks the given samples into batches.
    let private createBatches samples =
        samples
            |> Seq.toArray
            |> Array.randomShuffle
            |> Array.chunkBySize settings.AdvantageBatchSize
            |> Array.map (fun batch ->
                let inputs, targets, iters =
                    batch
                        |> Array.map (fun sample ->
                            sample.InfoSet,
                            sample.Regrets,
                            Seq.singleton sample.Weight)
                        |> Array.unzip3
                array2D inputs,
                array2D targets,
                array2D iters)

    /// Trains the given model using the given batches of
    /// data.
    let private trainBatches model batches
        (criterion : Loss<Tensor, Tensor, Tensor>)
        (optimizer : Optimizer) =
        Array.last [|
            for inputBatch, targetBatch, iterBatch in batches do

                    // move to GPU
                use inputs =
                    tensor(
                        (inputBatch : byte array2d),
                        device = settings.Device,
                        dtype = ScalarType.Float32)
                use targets =
                    tensor(
                        (targetBatch : float32 array2d),
                        device = settings.Device)
                use iters =
                    tensor(
                        (iterBatch : float32 array2d),
                        device = settings.Device)

                    // forward pass
                use loss =
                    use outputs = inputs --> model
                    use outputs' = iters * outputs   // favor later iterations
                    use targets' = iters * targets
                    criterion.forward(outputs', targets')

                    // backward pass and optimize
                optimizer.zero_grad()
                loss.backward()
                use _ = optimizer.step()

                loss.item<float32>()
        |]

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
                trainBatches model batches criterion optimizer
            settings.Writer.add_scalar(
                $"advantage loss/iter%03d{iter}",
                loss, epoch)
        model.eval()
