namespace Hearts.Generate

open System

open Hearts
open Hearts.Learn

module Trainer =

    /// Generates training data for the given iteration.
    let generateSamples settings iter state =

        /// TensorBoard logging.
        let log value step =
            settings.Writer.add_scalar(
                $"advantage samples/iter%03d{iter}",
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
                            Traverse.traverse settings iter deal rng)
                        |> Inference.complete
                            settings.TrainingSubBatchSize   // reuse setting for number of deals per inference chunk
                            state.ModelOpt

                    // save samples
                AdvantageSampleStore.writeSamples
                    samples state.SampleStore
                log
                    (float32 samples.Length / float32 numDeals)    // average number of generated samples per deal in this batch
                    (iBatch * settings.DealBatchSize + numDeals)   // total number of deals so far

                samples.Length
        |]
