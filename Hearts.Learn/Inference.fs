namespace Hearts.Learn

open Hearts
open Hearts.Model

type InferenceRequest = InformationSet

type InferenceManager() =

    let requests = ResizeArray<InferenceRequest>()

    member this.AddRequest(req) =
        requests.Add(req)

    member this.Infer() =
        Strategy.getFromAdvantage requests
