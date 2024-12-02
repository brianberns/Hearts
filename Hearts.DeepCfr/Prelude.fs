﻿namespace Hearts.DeepCfr

module Choice =

    /// Unzips a sequence of choices.
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

    /// Samples a strategy.
    let inline sample rng (strategy : Vector<_>) =
        let strategy' =
            strategy
                |> Seq.map float
                |> Seq.toArray
        Categorical.Sample(rng, strategy')
