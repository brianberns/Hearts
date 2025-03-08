namespace Hearts.Learn

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
