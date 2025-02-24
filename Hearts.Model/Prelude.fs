namespace Hearts.Model

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

module Array =

    open System.Threading.Tasks

    /// Maps in parallel with control over max degree of parallelism.
    let mapParallel maxDegreeOfParallelism mapping (array : _[]) =
        let result = Array.zeroCreate array.Length
        let options =
            ParallelOptions(
                MaxDegreeOfParallelism = maxDegreeOfParallelism)
        Parallel.For(0, array.Length, options, fun i ->
            result[i] <- mapping array[i])
                |> ignore
        result
