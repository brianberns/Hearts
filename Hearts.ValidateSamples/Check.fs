namespace Hearts.ValidateSamples

open System

/// Something wrong with a sample.
type Finding =
    {
        /// What kind of problem, e.g. "card in two places". Findings are counted by category.
        Category : string

        /// The particulars, for the first few samples in a category.
        Detail : string
    }

/// What the validator learned from one sample.
type SampleResult =
    {
        /// Problems found, if any.
        Findings : Finding list

        /// Was the sample taken while passing (rather than playing)?
        IsPass : bool

        /// Is the game score other than 0-0-0-0?
        HasGameScore : bool

        /// Absolute values of the stored (non-zero) regrets.
        AbsRegrets : float32[]

        /// Do the regrets sum to zero, as they must when the strategy that sampled the decision was
        /// uniform?
        IsBalanced : bool
    }

/// Checks one stored sample: that it decodes, that it describes a state a deal can reach under the
/// rules, and that its regrets fit that state.
module Check =

    let private numCards = 52
    let private numRanks = 13
    let private numCardsPerHand = 13
    let private numPointsPerDeal = 26
    let private endThreshold = 100

    let private hearts = 2
    let private twoOfClubs = 0
    let private queenOfSpades = 3 * numRanks + 10

    let private suitOf card = card / numRanks

    let private pointValue card =
        if suitOf card = hearts then 1
        elif card = queenOfSpades then 13
        else 0

    /// E.g. "♠".
    let suitName (suit : int) = string ("♣♦♥♠".[suit])

    /// E.g. "Q♠".
    let cardName card =
        string ("23456789TJQKA".[card % numRanks]) + suitName (suitOf card)

    let private cardNames cards =
        cards |> Seq.map cardName |> String.concat " "

    /// A seat, by its offset from the player.
    let private seatName offset =
        [| "the player"; "the next seat"; "the seat across"; "the previous seat" |][offset]

    /// Seat offset of the player who receives the player's pass, by encoded exchange direction
    /// (Left, Right, Across, Hold).
    let private receiverOffset = [| 1; 3; 2; 0 |]

    /// Reads one flag of a packed encoding.
    let getBit (buf : byte[]) offset i =
        (buf[offset + i / 8] >>> (i % 8)) &&& 1uy = 1uy

    /// Cards whose flags are set in the block of 52 at the given position.
    let private cardsAt (bits : bool[]) pos =
        [| for card in 0 .. numCards - 1 do if bits[pos + card] then card |]

    /// Decodes a thermometer: its value, or None if it isn't one (a set flag after a clear one).
    let private thermometer (bits : bool[]) pos width =
        let value =
            Seq.init width (fun i -> bits[pos + i])
                |> Seq.takeWhile id
                |> Seq.length
        if Seq.init (width - value) (fun i -> bits[pos + value + i]) |> Seq.exists id then None
        else Some value

    /// E.g. "13-0-0-0".
    let private scoreText (values : int[]) =
        values |> Array.map string |> String.concat "-"

    let private finding category detail =
        { Category = category; Detail = detail }

    /// Checks the sample at the given offset of the given buffer. Regrets may not exceed the given
    /// bound, which depends on the payoff level.
    let check (layout : Layout) (regretBound : float32) (buf : byte[]) offset : SampleResult =

            // decode the encoding
        let bits = Array.init layout.NumInputs (getBit buf offset)
        let paddingSet =
            seq { layout.NumInputs .. layout.EncodingBytes * 8 - 1 }
                |> Seq.exists (getBit buf offset)

            // decode the regrets: (card index, regret) pairs, then padding (255, 0)
        let regretPos = offset + layout.EncodingBytes
        let entries =
            [|
                for j = 0 to Layout.MaxRegrets - 1 do
                    let pos = regretPos + j * 5
                    buf[pos], BitConverter.ToSingle(buf, pos + 1)
            |]
        let regrets =
            entries
                |> Array.takeWhile (fun (idx, _) -> idx <> Byte.MaxValue)
                |> Array.map (fun (idx, r) -> int idx, r)
        let regretFindings =
            [
                let padding = entries[regrets.Length ..]
                if padding |> Array.exists (fun (idx, r) -> idx <> Byte.MaxValue || r <> 0f) then
                    finding "malformed regret padding"
                        "an entry after the first padding entry isn't padding (255, 0)"
                for idx, r in regrets do
                    if idx >= numCards then
                        finding "regret card index out of range" $"card index {idx}"
                    if r = 0f then
                        finding "zero regret stored" $"card {cardName (min idx 51)}"
                    if Single.IsNaN(r) || Single.IsInfinity(r) then
                        finding "non-finite regret" $"{r} for card index {idx}"
                    elif abs r > regretBound then
                        finding "regret out of range"
                            $"{r} for {cardName (min idx 51)}: beyond ±{regretBound} for this payoff level"
                let indexes = regrets |> Array.map fst
                if (Array.distinct indexes).Length < indexes.Length then
                    finding "duplicate regret card" $"card indexes {scoreText indexes}"
            ]
        let absRegrets = regrets |> Array.map (snd >> abs)
        let isBalanced =
            let scale = if absRegrets.Length = 0 then 1f else max 1f (Array.max absRegrets)
            abs (regrets |> Array.sumBy snd) <= 1e-3f * scale
        let result findings isPass hasGameScore =
            {
                Findings = findings
                IsPass = isPass
                HasGameScore = hasGameScore
                AbsRegrets = absRegrets
                IsBalanced = isBalanced
            }

            // structure: the parts must decode before the state can be checked
        let directions = [| for d in 0 .. 3 do if bits[layout.Direction + d] then d |]
        let trickSlots = Array.init 3 (fun i -> cardsAt bits (layout.Trick + i * numCards))
        let trickLength = trickSlots |> Array.takeWhile (fun s -> s.Length = 1) |> Array.length
        let dealScores =
            Array.init 4 (fun j ->
                thermometer bits (layout.DealScore + j * layout.DealScoreWidth) layout.DealScoreWidth)
        let gameScores =
            Array.init 4 (fun j ->
                thermometer bits (layout.GameScore + j * layout.GameScoreWidth) layout.GameScoreWidth)
        let structureFindings =
            [
                if paddingSet then
                    finding "padding flag set" "a flag past the end of the encoding is set"
                if directions.Length <> 1 then
                    let allZero = seq { offset .. offset + layout.SampleBytes - 1 } |> Seq.forall (fun i -> buf[i] = 0uy)
                    if allZero then finding "zero-filled sample" "every byte is zero"
                    else finding "exchange direction not one-hot" $"directions set: {scoreText directions}"
                if trickSlots |> Array.exists (fun s -> s.Length > 1)
                    || trickSlots[trickLength ..] |> Array.exists (fun s -> s.Length > 0) then
                    finding "malformed current trick"
                        $"slots hold {trickSlots |> Array.map (fun s -> s.Length) |> scoreText} cards"
                if dealScores |> Array.exists Option.isNone then
                    finding "deal score not a thermometer" "a set flag follows a clear one"
                if gameScores |> Array.exists Option.isNone then
                    finding "game score not a thermometer" "a set flag follows a clear one"
            ]
        if structureFindings |> List.exists (fun f -> f.Category = "zero-filled sample") then
            result structureFindings false false
        elif not structureFindings.IsEmpty then
            result (structureFindings @ regretFindings) false false
        else

        let direction = directions[0]
        let isHold = (direction = 3)
        let hand = cardsAt bits layout.Hand
        let outgoing = cardsAt bits layout.OutgoingPass
        let incoming = cardsAt bits layout.IncomingPass
        let plays = Array.init 4 (fun j -> cardsAt bits (layout.Plays + j * numCards))
        let trick = trickSlots[.. trickLength - 1] |> Array.map Array.exactlyOne
        let isVoid offset suit = bits[layout.Voids + suit * 3 + offset - 1]
        let dealScores = dealScores |> Array.map Option.get
        let gameScores = gameScores |> Array.map Option.get
        let hasGameScore = gameScores |> Array.exists (fun v -> v > 0)
        let isPass = not isHold && outgoing.Length < 3

            // game score: the game isn't over, and each deal adds 26 points in all (78 for a moon,
            // under AlwaysAddToOthers)
        let gameScoreFindings =
            [
                if layout.GameScoreWidth > 0 then
                    let saturated = gameScores |> Array.exists (fun v -> v = layout.GameScoreWidth)
                    if not saturated && Array.sum gameScores % numPointsPerDeal <> 0 then
                        finding "game score total not a multiple of 26" $"game score {scoreText gameScores}"
                    let low = Array.min gameScores
                    if Array.max gameScores >= endThreshold
                        && (gameScores |> Array.filter ((=) low)).Length = 1 then
                        finding "game already over" $"game score {scoreText gameScores}"
            ]

            // every card is in at most one of: the hand, a seat's completed plays, the current trick
        let placed =
            [|
                for card in hand -> card, "the hand"
                for j in 0 .. 3 do
                    for card in plays[j] -> card, $"the plays of {seatName j}"
                for card in trick -> card, "the current trick"
            |]
        let placementFindings =
            [
                for card, places in placed |> Array.groupBy fst do
                    if places.Length > 1 then
                        finding "card in two places"
                            $"""{cardName card} in {places |> Array.map snd |> String.concat " and "}"""
            ]

        let numTricks = plays[0].Length
        let points = plays |> Array.sumBy (Array.sumBy pointValue)

        let stateFindings =
            if isPass then
                [
                    if incoming.Length > 0 then
                        finding "pass received while passing" $"incoming {cardNames incoming}"
                    if plays |> Array.exists (fun p -> p.Length > 0) || trick.Length > 0 then
                        finding "cards played while passing" $"{numTricks} tricks, {trick.Length} in the current one"
                    if hand.Length + outgoing.Length <> numCardsPerHand then
                        finding "wrong hand size"
                            $"{hand.Length} cards in hand and {outgoing.Length} passed while passing"
                    if Array.exists (fun c -> Array.contains c hand) outgoing then
                        finding "passed card still in hand" (cardNames outgoing)
                    if Array.sum dealScores > 0 then
                        finding "deal score while passing" $"deal score {scoreText dealScores}"
                    if Seq.exists (fun i -> bits[layout.Voids + i]) (seq { 0 .. 11 }) then
                        finding "void while passing" "a void is set before any card is played"
                ]
            else
                [
                        // passes
                    if isHold && (outgoing.Length > 0 || incoming.Length > 0) then
                        finding "pass on a hold deal"
                            $"outgoing {cardNames outgoing}, incoming {cardNames incoming}"
                    if not isHold && incoming.Length <> 3 then
                        finding "incomplete incoming pass" $"incoming {cardNames incoming}"
                    if Array.exists (fun c -> Array.contains c incoming) outgoing then
                        finding "card passed both ways" (cardNames outgoing)
                    for card in incoming do
                        if not (Array.contains card hand || Array.contains card plays[0]) then
                            finding "received card missing" $"{cardName card} is neither in the hand nor played by the player"

                        // completed tricks and the hand
                    if plays |> Array.exists (fun p -> p.Length <> numTricks) then
                        finding "uneven completed tricks"
                            $"seats have played {plays |> Array.map Array.length |> scoreText} cards in completed tricks"
                    if numTricks >= numCardsPerHand then
                        finding "deal already complete" $"{numTricks} tricks completed"
                    if hand.Length <> numCardsPerHand - numTricks then
                        finding "wrong hand size" $"{hand.Length} cards in hand after {numTricks} tricks"

                        // the first trick
                    if numTricks = 0 && trick.Length > 0 && trick[0] <> twoOfClubs then
                        finding "first trick not led with the 2♣" $"led {cardName trick[0]}"
                ]

        if not (placementFindings.IsEmpty && stateFindings.IsEmpty) then
            result (structureFindings @ placementFindings @ stateFindings @ gameScoreFindings @ regretFindings)
                isPass hasGameScore
        else

            // the rest needs a consistent play state; who played each card of the current trick
        let trickOffset i = (4 - trick.Length + i) % 4
        let receiver = receiverOffset[direction]
        let knownHeld =   // the player's passed cards that the receiver still holds
            outgoing
                |> Array.filter (fun card ->
                    not (Array.contains card plays[receiver])
                        && not (Array.contains card trick))
        let playFindings =
            if isPass then []
            else
                [
                        // the receiver alone can have played the player's passed cards
                    for card in outgoing do
                        if Array.contains card hand || Array.contains card plays[0] then
                            finding "passed card still held" $"{cardName card} passed but in the player's hand or plays"
                        for j in 1 .. 3 do
                            if j <> receiver && Array.contains card plays[j] then
                                finding "passed card played by the wrong seat"
                                    $"{cardName card} passed to {seatName receiver} but played by {seatName j}"
                    for i, card in Array.indexed trick do
                        if Array.contains card outgoing && trickOffset i <> receiver then
                            finding "passed card played by the wrong seat"
                                $"{cardName card} passed to {seatName receiver} but played by {seatName (trickOffset i)}"

                        // a seat that didn't follow suit in the current trick is void in the suit led
                    if trick.Length > 0 then
                        let led = suitOf trick[0]
                        for i in 1 .. trick.Length - 1 do
                            if suitOf trick[i] <> led && not (isVoid (trickOffset i) led) then
                                finding "missing void"
                                    $"{seatName (trickOffset i)} played {cardName trick[i]} on a {suitName led} lead but isn't void"

                        // a seat known to hold a passed card isn't void in its suit
                    for card in knownHeld do
                        if isVoid receiver (suitOf card) then
                            finding "void contradicts pass"
                                $"{seatName receiver} holds {cardName card} but is void in {suitName (suitOf card)}"

                        // voids leave room for the unseen cards (Hall's condition over sets of suits)
                    let remaining j =
                        numCardsPerHand - numTricks
                            - (if Array.exists (fun i -> trickOffset i = j) [| 0 .. trick.Length - 1 |] then 1 else 0)
                    let capacity j =
                        remaining j - (if j = receiver then knownHeld.Length else 0)
                    let seen =
                        Array.concat [ hand; Array.concat plays; trick; knownHeld ]
                    let unseen suit =
                        numRanks - (seen |> Array.filter (fun c -> suitOf c = suit)).Length
                    for subset in 1 .. 15 do
                        let suits = [| for s in 0 .. 3 do if subset &&& (1 <<< s) <> 0 then s |]
                        let need = suits |> Array.sumBy unseen
                        let room =
                            [ 1 .. 3 ]
                                |> List.filter (fun j -> suits |> Array.exists (fun s -> not (isVoid j s)))
                                |> List.sumBy capacity
                        if need > room then
                            finding "voids leave no room for unseen cards"
                                $"""{need} unseen {suits |> Array.map suitName |> String.concat ""} but room for {room}"""

                        // deal score: the points in completed tricks
                    if layout.DealScoreWidth = 1 then
                        if (Array.sum dealScores > 0) <> (points > 0) then
                            finding "deal score doesn't match tricks"
                                $"flags {scoreText dealScores}, {points} points in completed tricks"
                    elif Array.sum dealScores <> points then
                        finding "deal score doesn't match tricks"
                            $"deal score {scoreText dealScores}, {points} points in completed tricks"

                        // traversal ends a deal at a trick boundary once all points are taken
                    if trick.Length = 0 && points = numPointsPerDeal then
                        finding "sample after all points were taken" $"{numTricks} tricks completed"
                ]

            // legal actions
        let legal =
            if isPass then hand
            else
                match numTricks, trick with
                    | 0, [||] -> [| twoOfClubs |]
                    | _, [||] ->
                        let played = Array.append (Array.concat plays) trick
                        let broken =
                            played |> Array.exists (fun c ->
                                suitOf c = hearts || (layout.QueenBreaksHearts && c = queenOfSpades))
                        let nonHearts = hand |> Array.filter (fun c -> suitOf c <> hearts)
                        if broken || nonHearts.Length = 0 then hand else nonHearts
                    | iTrick, _ ->
                        let led = suitOf trick[0]
                        let following = hand |> Array.filter (fun c -> suitOf c = led)
                        let cards = if following.Length > 0 then following else hand
                        let noPoints = cards |> Array.filter (fun c -> pointValue c = 0)
                        if iTrick = 0 && noPoints.Length > 0 then noPoints else cards
        let legalFindings =
            [
                if legal.Length < 2 then
                    finding "sample at a forced action" $"only legal action: {cardNames legal}"
                for idx, _ in regrets do
                    if idx < numCards && not (Array.contains idx legal) then
                        finding "regret for an illegal action"
                            $"{cardName idx}; legal: {cardNames legal}"

                    // the info set's utility averages its actions' utilities, so its regrets can't all
                    // have one sign
                let legalRegrets =
                    legal |> Array.map (fun c ->
                        regrets |> Array.tryFind (fst >> (=) c) |> Option.map snd |> Option.defaultValue 0f)
                if legalRegrets.Length > 0 then
                    let tolerance = 1e-4f * max 1f (legalRegrets |> Array.map abs |> Array.max)
                    if Array.min legalRegrets > tolerance || Array.max legalRegrets < -tolerance then
                        finding "regrets all one sign"
                            $"""{legal |> Array.mapi (fun i c -> $"{cardName c} {legalRegrets[i]}") |> String.concat ", "}"""
            ]

        result (playFindings @ gameScoreFindings @ legalFindings @ regretFindings) isPass hasGameScore
