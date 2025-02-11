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

    /// Trains the given model using the given samples.
    let train samples (model : AdvantageModel) =

            // prepare training data
        let data =
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

            // train model
        use optimizer =
            Adam(
                model.parameters(),
                settings.LearningRate)
        use loss = MSELoss()
        model.train()
        let losses =
            [|
                for _ = 1 to settings.NumAdvantageTrainEpochs do
                    Array.last [|
                        for inputArray, targetArray, iterArray in data do

                                // move to GPU
                            use inputs = tensor(inputArray, device = settings.Device)
                            use targets = tensor(targetArray, device = settings.Device)
                            use iters = tensor(iterArray, device = settings.Device)

                                // forward pass
                            use loss =
                                use outputs = inputs --> model
                                use outputs' = iters * outputs   // favor later iterations
                                use targets' = iters * targets
                                loss.forward(outputs', targets')

                                // backward pass and optimize
                            optimizer.zero_grad()
                            loss.backward()
                            use _ = optimizer.step()

                            loss.item<float32>()
                    |]
            |]
        model.eval()

        losses
