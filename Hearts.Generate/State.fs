namespace Hearts.Generate

open System

open Hearts.Learn
open Hearts.Model

/// Advantage state.
type AdvantageState =
    {
        /// Current advantage model, if any.
        AdvantageModelOpt : Option<AdvantageModel>

        /// Stored training data.
        SampleStore : AdvantageSampleStore
    }

    /// Cleanup.
    member this.Dispose() =
        this.AdvantageModelOpt
            |> Option.iter _.Dispose()
        this.SampleStore.Dispose()

    interface IDisposable with

        /// Cleanup.
        member this.Dispose() =
            this.Dispose()

module AdvantageState =

    /// Creates an advantage state.
    let create advantageModelOpt sampleStore =
        {
            AdvantageModelOpt = advantageModelOpt
            SampleStore = sampleStore
        }
