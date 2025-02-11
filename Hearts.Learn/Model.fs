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

        /// 1-based iteration number.
        Iteration : int
    }

module AdvantageSample =

    /// Creates an advantage sample.
    let create hand deal (regrets : Vector<_>) iteration =
        assert(regrets.Count = Network.outputSize)
        assert(iteration > 0)
        {
            InfoSet = Encoding.encode hand deal
            Regrets = regrets
            Iteration = iteration
        }

module Tensor =
     
    /// Converts the given rows to a tensor.
    let ofSeq (rows : seq<#seq<float32>>) =
        tensor(array2D rows, device = settings.Device)

module AdvantageModel =

    /// Trains the given model using the given samples.
    let train samples (model : AdvantageModel) =

            // prepare training data
        let tensors =
            samples
                |> Seq.toArray
                |> Array.randomShuffle
                |> Array.chunkBySize settings.AdvantageBatchSize
                |> Array.map (fun batch ->
                    let inputs, targets, iters =
                        batch
                            |> Array.map (fun sample ->
                                let input = sample.InfoSet
                                let target = sample.Regrets
                                let iter =
                                    sample.Iteration
                                        |> float32
                                        |> sqrt
                                        |> Seq.singleton
                                input, target, iter)
                            |> Array.unzip3
                    Tensor.ofSeq inputs,
                    Tensor.ofSeq targets,
                    Tensor.ofSeq iters)

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
                        for inputs, targets, iters in tensors do

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

            // cleanup
        for inputs, targets, iters in tensors do
            inputs.Dispose()
            targets.Dispose()
            iters.Dispose()

        losses
