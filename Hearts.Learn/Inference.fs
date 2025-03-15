namespace Hearts.Learn

open Hearts
open Hearts.Model

type InferenceRequest = InformationSet

type InferenceManager(model) =

    let requests = ResizeArray<InferenceRequest>()

    member _.AddRequest(req) =
        requests.Add(req)

    member _.Infer() =
        Strategy.getFromAdvantage requests model
