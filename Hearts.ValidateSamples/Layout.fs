namespace Hearts.ValidateSamples

/// The bit layout of a stored sample, for one version of the encoding. The validator reads stores
/// with its own description of their layout, not with the code that wrote them, so that a bug in that
/// code can't hide itself.
type Layout =
    {
        /// Short description of this version.
        Name : string

        /// Number of encoded flags.
        NumInputs : int

        /// Offsets of the encoding's parts.
        Hand : int
        Direction : int
        OutgoingPass : int
        IncomingPass : int
        Plays : int           // completed tricks: one block of cards per seat, the player first
        Trick : int           // current trick: one block of cards per earlier play, in order
        Voids : int           // other seats' voids: suit * 3 + (seat offset - 1)
        DealScore : int
        GameScore : int

        /// Flags per seat of the deal score: 1 (has taken points) or a thermometer.
        DealScoreWidth : int

        /// Flags per seat of the game score: 0 (none) or a thermometer.
        GameScoreWidth : int

        /// Does the Q♠ also break hearts? (Stores written before 10f0996 used that rule under
        /// Rules.arena.)
        QueenBreaksHearts : bool
    }

    /// Number of bytes in a packed encoding.
    member layout.EncodingBytes =
        (layout.NumInputs + 7) / 8

    /// Number of bytes in a stored sample: the packed encoding, then 13 (card index, regret) pairs.
    member layout.SampleBytes =
        layout.EncodingBytes + Layout.RegretBytes

    /// Maximum number of stored regrets: one per card in a hand.
    static member MaxRegrets = 13

    /// Number of bytes of stored regrets.
    static member RegretBytes = Layout.MaxRegrets * (1 + 4)

module Layout =

    /// Number of bytes in a store's header: "Hrts" and the iteration number.
    let headerBytes = 8

    let private create name dealScoreWidth gameScoreWidth queenBreaksHearts =
        let hand = 0
        let direction = hand + 52
        let outgoing = direction + 4
        let incoming = outgoing + 52
        let plays = incoming + 52
        let trick = plays + 4 * 52
        let voids = trick + 3 * 52
        let dealScore = voids + 3 * 4
        let gameScore = dealScore + 4 * dealScoreWidth
        {
            Name = name
            NumInputs = gameScore + 4 * gameScoreWidth
            Hand = hand
            Direction = direction
            OutgoingPass = outgoing
            IncomingPass = incoming
            Plays = plays
            Trick = trick
            Voids = voids
            DealScore = dealScore
            GameScore = gameScore
            DealScoreWidth = dealScoreWidth
            GameScoreWidth = gameScoreWidth
            QueenBreaksHearts = queenBreaksHearts
        }

    /// The encoding before ad5bebe: 540 flags, each seat's deal score as one flag (has taken
    /// points), no game score.
    let v1 = create "540 inputs (before ad5bebe): no game score" 1 0 true

    /// The encoding since ad5bebe: 1,144 flags, deal and game scores as thermometers.
    let v2 = create "1,144 inputs (since ad5bebe): deal and game score thermometers" 26 126 false

    /// The layouts the validator knows, newest first.
    let all = [ v2; v1 ]
