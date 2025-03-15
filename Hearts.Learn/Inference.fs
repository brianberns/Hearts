﻿namespace Hearts.Learn

open Hearts
open Hearts.Model

type private Request = InformationSet
type private Response = MathNet.Numerics.LinearAlgebra.Vector<float32>

type InferenceManager(model) =

    let requests = ResizeArray<Request>()
    let responses = ResizeArray<Response>()

    member _.Model = model

    member _.AddRequest(req) =
        let idx = requests.Count
        requests.Add(req)
        idx

    member _.Infer() =
        Strategy.getFromAdvantage requests model
            |> responses.AddRange

    member _.GetResponse(idx) =
        responses[idx]
