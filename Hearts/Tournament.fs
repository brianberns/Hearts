namespace Hearts

open System
open PlayingCards
open Hearts

/// SplitMix64 behind the System.Random API, by Claude. This is
/// not thread-safe.
/// https://rosettacode.org/wiki/Pseudo-random_numbers/Splitmix64
type SplitMixRandom(seed : uint64) =
    inherit Random()

    let mutable state = seed

    let nextU64 () =
        state <- state + 0x9E3779B97F4A7C15UL
        let z = (state ^^^ (state >>> 30)) * 0xBF58476D1CE4E5B9UL
        let z = (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
        z ^^^ (z >>> 31)

    /// Unbiased integer in [0, n), by rejection (no modulo bias).
    let nextBelow (n : int) =
        let n = uint64 n
        let limit = UInt64.MaxValue - UInt64.MaxValue % n
        let rec loop () =
            let x = nextU64 ()
            if x < limit then int (x % n) else loop ()
        loop ()

    override _.Next(maxValue) =
        nextBelow maxValue

    override _.Next(minValue, maxValue) =
        minValue + nextBelow (maxValue - minValue)

    override _.Next() =
        nextBelow Int32.MaxValue

    override _.NextDouble() =
        float (nextU64 () >>> 11) * (1.0 / 9007199254740992.0)

    override _.Sample() =
        float (nextU64 () >>> 11) * (1.0 / 9007199254740992.0)

    /// Distinct, well-mixed seed for offset i of a run.
    static member GetSeed(runSeed : int, iOffset : int) =
        let x = (uint64 runSeed <<< 32) + uint64 iOffset + 0x9E3779B97F4A7C15UL
        let z = (x ^^^ (x >>> 30)) * 0xBF58476D1CE4E5B9UL
        let z = (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
        z ^^^ (z >>> 31)

module Tournament =

    /// Runs a deterministic tournament between two players.
    let run rngSeed inParallel numGames champion challenger =

            // prepare players
        let challengerSeat = Seat.South
        let playerMap =
            Enum.getValues<Seat>
                |> Seq.map (fun seat ->
                    let player =
                        if seat = challengerSeat then challenger
                        else champion
                    seat, player)
                |> Map

            // run games
        let games =
            let rng = Random(rngSeed)
            Game.playGames rng inParallel numGames (fun iGame game ->
                let rng =
                    SplitMixRandom.GetSeed(rngSeed, iGame)   // give each game its own RNG
                        |> SplitMixRandom
                Game.playGame Rules.arena rng playerMap game)

            // compute results
        let winnerMap =
            games
                |> Seq.collect (fun game ->
                    GameScore.findGameWinners game.Score)
                |> Seq.groupBy id
                |> Seq.map (fun (seat, group) ->
                    seat, Seq.length group)
                |> Map
        let score =
            Score.ofPoints [|
                for seat in Enum.getValues<Seat> do
                    match Map.tryFind seat winnerMap with
                        | Some wins -> wins
                        | None -> 0
            |]
        float32 score[challengerSeat] / float32 numGames
