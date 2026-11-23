namespace Hearts

open System
open PlayingCards
open Hearts

/// SplitMix64 behind the System.Random API, by Claude. Every
/// public method draws from the SplitMix64 stream, so a given
/// seed always produces the same results. This is not thread-safe.
/// https://rosettacode.org/wiki/Pseudo-random_numbers/Splitmix64
type SplitMixRandom(seed : uint64) =
    inherit Random()

    /// SplitMix64 increment ("golden gamma").
    static let gamma = 0x9E3779B97F4A7C15UL

    /// SplitMix64 output mixer. A bijection on 64-bit values.
    static let mix (x : uint64) =
        let z = (x ^^^ (x >>> 30)) * 0xBF58476D1CE4E5B9UL
        let z = (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
        z ^^^ (z >>> 31)

    /// 2^-53, for converting 53 random bits to [0, 1).
    static let doubleUnit = 1.0 / 9007199254740992.0

    let mutable state = seed

    let nextU64 () =
        state <- state + gamma
        mix state

    /// Unbiased integer in [0, n), by rejection (no modulo bias).
    /// Answers 0 if n is 0 or 1.
    let nextBelow (n : uint64) =
        if n <= 1UL then 0UL
        else
            let limit = UInt64.MaxValue - UInt64.MaxValue % n   // multiple of n
            let rec loop () =
                let x = nextU64 ()
                if x < limit then x % n else loop ()
            loop ()

    /// Unbiased integer in [minValue, maxValue), assuming minValue <= maxValue.
    let nextInRange (minValue : int64) (maxValue : int64) =
        let range = uint64 maxValue - uint64 minValue   // wraps correctly for any int64 range
        minValue + int64 (nextBelow range)              // wraps back into range

    /// Random double in [0, 1).
    let nextDouble () =
        float (nextU64 () >>> 11) * doubleUnit

    override _.Next(maxValue) =
        ArgumentOutOfRangeException.ThrowIfNegative(maxValue, nameof maxValue)
        int (nextBelow (uint64 maxValue))

    override _.Next(minValue, maxValue) =
        ArgumentOutOfRangeException.ThrowIfGreaterThan(minValue, maxValue, nameof minValue)
        int (nextInRange (int64 minValue) (int64 maxValue))

    override _.Next() =
        int (nextBelow (uint64 Int32.MaxValue))

    override _.NextInt64(maxValue : int64) =
        ArgumentOutOfRangeException.ThrowIfNegative(maxValue, nameof maxValue)
        int64 (nextBelow (uint64 maxValue))

    override _.NextInt64(minValue : int64, maxValue : int64) =
        ArgumentOutOfRangeException.ThrowIfGreaterThan(minValue, maxValue, nameof minValue)
        nextInRange minValue maxValue

    override _.NextInt64() =
        int64 (nextBelow (uint64 Int64.MaxValue))

    override _.NextDouble() =
        nextDouble ()

    override _.NextSingle() =
        float32 (nextU64 () >>> 40) * (1.0f / 16777216.0f)   // 24 random bits

    override _.Sample() =
        nextDouble ()

    override _.NextBytes(buffer : Span<byte>) =
        let mutable i = 0
        while i < buffer.Length do
            let x = nextU64 ()
            let n = min 8 (buffer.Length - i)
            for k = 0 to n - 1 do
                buffer[i + k] <- byte (x >>> (8 * k))
            i <- i + n

    override this.NextBytes(buffer : byte[]) =
        ArgumentNullException.ThrowIfNull(buffer, nameof buffer)
        this.NextBytes(Span<byte>(buffer))

    /// Distinct, well-mixed seed for offset i of a run. Distinct
    /// (runSeed, iOffset) pairs always produce distinct seeds.
    static member GetSeed(runSeed : int, iOffset : int) =
        let x =
            (uint64 (uint32 runSeed) <<< 32)   // no sign extension
                ||| uint64 (uint32 iOffset)
        mix (x + gamma)

    /// Distinct, well-mixed seed for child i of the given seed, as for the
    /// players in a rotation. Children start far from the stream that
    /// SplitMixRandom(seed) itself produces, so neither shares numbers.
    static member GetChildSeed(seed : uint64, i : int) =
        let start = mix (seed ^^^ 0x5851F42D4C957F2DUL)   // leave seed's own stream
        mix (start + gamma * (uint64 (uint32 i) + 1UL))

module Tournament =

    /// Runs a deterministic tournament between two players
    /// using duplicate deals, answering the challenger's
    /// win rate.
    let runSeeded rngSeed numGames makeChampion makeChallenger =

        /// Creates a player.
        let createPlayer rotationSeed offset makePlayer =
            let seed =
                SplitMixRandom.GetChildSeed(rotationSeed, offset)
            makePlayer seed

        /// Puts challenger in the given seat.
        let createPlayerMap champion challenger challengerSeat =
            Enum.getValues<Seat>
                |> Seq.map (fun seat ->
                    let player =
                        if seat = challengerSeat then challenger
                        else champion
                    seat, player)
                |> Map

            // determine number of rotations needed
        let numRotations = numGames / Seat.numSeats
        assert(numRotations > 0)

            // count number of times challenger wins
        let nWins =

                // rotate challenger through each seat
            Array.Parallel.init numRotations (fun iRotation ->
                let rotationSeed = SplitMixRandom.GetSeed(rngSeed, iRotation)
                let champion = createPlayer rotationSeed 0 makeChampion
                let challenger = createPlayer rotationSeed 1 makeChallenger
                Seat.cycle Seat.South
                    |> Seq.sumBy (fun challengerSeat ->
                        let playerMap =
                            createPlayerMap champion challenger challengerSeat
                        let rng = SplitMixRandom(rotationSeed)                   // restart RNG for duplicate deals
                        let isWinner =
                            Game.playGame Rules.arena rng playerMap Seat.South   // players rotate seats, so first dealer can be constant
                                |> GameScore.findGameWinners Rules.arena.TieGame
                                |> Set.contains challengerSeat
                        if isWinner then 1 else 0))
                |> Array.sum

            // compute results
        let numGames = numRotations * Seat.numSeats
        float32 nWins / float32 numGames

    /// Runs a deterministic tournament between two players
    /// using duplicate deals, answering the challenger's
    /// win rate.
    let run rngSeed numGames champion challenger =
        runSeeded
            rngSeed numGames
            (fun _ -> champion)
            (fun _ -> challenger)
