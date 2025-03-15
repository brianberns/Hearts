namespace Hearts.Learn

open Hearts

type InferenceRequest = InformationSet

type InferenceManager() =

    let requests = ResizeArray<InferenceRequest>()

    member this.AddRequest(req) =
        requests.Add(req)
