namespace Hearts.Generate

open System

open Hearts.Learn
open Hearts.Model

/// Advantage state.
type AdvantageState =
    {
        /// Current model, if any.
        ModelOpt : Option<AdvantageModel>

        /// Stored training data.
        SampleStore : AdvantageSampleStore
    }

    /// Cleanup.
    member this.Dispose() =
        this.ModelOpt
            |> Option.iter _.Dispose()
        this.SampleStore.Dispose()

    interface IDisposable with

        /// Cleanup.
        member this.Dispose() =
            this.Dispose()

module AdvantageState =

    /// Creates an advantage state.
    let create modelOpt sampleStore =
        {
            ModelOpt = modelOpt
            SampleStore = sampleStore
        }
