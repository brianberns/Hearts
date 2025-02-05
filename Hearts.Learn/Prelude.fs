﻿namespace Hearts.Learn

module Choice =

    /// Unzips an array of choices.
    let unzip choices =
        let opts =
            choices
                |> Array.map (function
                    | Choice1Of2 ch -> Some ch, None
                    | Choice2Of2 ch -> None, Some ch)
        Array.choose fst opts,
        Array.choose snd opts

module Vector =

    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Distributions

    /// Gets an element from a vector.
    let get (vector : Vector<_>) index =
        vector[index]

    /// Samples a vector.
    let inline sample rng (vector : Vector<_>) =
        let vector' =
            vector
                |> Seq.map float
                |> Seq.toArray
        Categorical.Sample(rng, vector')
