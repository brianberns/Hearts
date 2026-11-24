namespace Hearts.Heuristic.KillerEngine

open System

/// What a seat's decision leaves behind in Killer Hearts' shared state, for its later inferences
/// and decisions.
type Memory =
    {
        Mode : Mode
        ImScore : int[]
        LowScore : int
        HighScore : int
        LeadOver2ndLow : int
        CanLeadH : bool
        SluffPts : bool
        Shooter : KSeat
        PossOppShoot : bool
        ProbOppShoot : bool
        /// The seat that tried to Shoot, if any (Shoot_try).
        ShootTry : KSeat
        /// The seat's private knowledge of the others' cards, by seat, rank and suit.
        PrivateInfo : int[,,]
    }

/// What a Killer Hearts player at one seat knows of the deal: the public record of play, the public
/// inferences every seat draws from it, and its own private knowledge (its hand and its pass). This
/// is the bookkeeping of Killer Hearts' form (play_trick, analyze_trick_so_far and
/// update_void_records_and_other_parameters), seen from one seat.
///
/// A table is built by replaying a deal, one play at a time. Only the methods here change it, apart
/// from what the seat's own decisions leave behind (its mode, score estimates and Shoot suspicions),
/// which the seat records with Remember.
///
/// The game score is each seat's score before the deal, by seat, none of them 100 or more (see
/// killerScores).
[<AllowNullLiteral>]
type Table(me : KSeat, dealer : KSeat, passDirection : int, gameScore : int[], held : KCard[], passed : KCard[],
           received : KCard[], playedBy : KSeat -> KCard[]) =

    let cardsPlayed = Array2D.zeroCreate<bool> 15 4
    let countPlayed = Array.zeroCreate<int> 4
    let hand = Array2D.zeroCreate<bool> 15 4
    let playerHasPlayed = Array.zeroCreate<bool> 4
    let scoreInHand = Array.zeroCreate<int> 4

    /// Game scores before the deal, lowest first, from index 1 (low) to 4 (high), as Killer Hearts'
    /// score_between_hands.
    let scoreBetweenHands = Array.append [| 0 |] (Array.sort gameScore)

    /// Each seat's game score plus the points it has taken in the deal, before the current trick
    /// (score_in_game).
    let scoreInGame = Array.copy gameScore
    let tricksWonBy = Array.zeroCreate<int> 4
    let imScore = Array.zeroCreate<int> 4
    let pubLowestUnplayed = Array.create 4 2
    let handSize = Array.create 4 13
    let isVoid = Array2D.zeroCreate<bool> 4 4
    let voidInSuit = Array.zeroCreate<int> 4
    let suitPulls = Array.zeroCreate<int> 4
    let highLeadsPerSuit = Array.zeroCreate<int> 4
    let suitSluffs = Array2D.zeroCreate<int> 4 4
    let playerSluffs = Array.zeroCreate<int> 4
    let hasLedSpades = Array.zeroCreate<int> 4
    let playedAKS = Array.zeroCreate<bool> 4
    let hasSluffed = Array.zeroCreate<bool> 4
    let hasSluffedAKS = Array.zeroCreate<bool> 4
    let publicInfo = Array3D.zeroCreate<int> 4 15 4
    let privateInfo = Array3D.zeroCreate<int> 4 15 4
    let pubTgt = Array2D.zeroCreate<bool> 4 4
    let arcShooter = Array.create 4 none
    let arcPossOppShoot = Array.zeroCreate<bool> 4
    let arcProbOppShoot = Array.zeroCreate<bool> 4

    let mutable trick = 0
    let mutable position = 0
    let mutable player = none
    let mutable trickLeader = none
    let mutable suitLed = none
    let mutable w = none
    let mutable trickWinner = none
    let mutable prevW = none
    let mutable prevTrickWinner = none
    let mutable pointsInTrick = 0
    let mutable pointsPlayed = 0
    let mutable pointsUnplayed = 26
    let mutable pointsSit = PointsSit.unplayed
    let mutable qsOnTrick = false
    let mutable qsLedOnTrick = false
    let mutable qsPlayed = false
    let mutable playerPlayedQS = none
    let mutable cardLed = none
    let mutable playsAKSInTrick = none
    let mutable sluffs = 0
    let mutable aksPlayed = 0
    let mutable canLeadH = false
    let mutable sluffPts = false
    let mutable twoCLeads = true
    let mutable lowScore = 0
    let mutable highScore = 0
    let mutable leadOver2ndLow = 0
    let mutable pNotLeadingS = none
    let mutable ledAKS = none
    let mutable followsWithAKSOnFirstPull = none
    let mutable followsWithQSOnFirstPull = none
    let mutable possibleQSPoison = none
    let mutable mode = Mode.NoMode
    let mutable shootTry = none
    let mutable pubFollowersNotVoid = 0
    let mutable pubQSOwner = 0   // as Killer Hearts leaves it until the first play
    let mutable pubQSOwnerPlayed = false
    let mutable pubHelpShooter = false

    /// The seat Killer Hearts' one-seat table believes holds Q♠ at the start of play. The table fills
    /// the other seats' unknown cards arbitrarily, in card-number order, so unless Q♠ is in this
    /// seat's hand, its pass, or already played, this is an artifact of that filling.
    let ownsQS =
        let qs = { Rank = queen; Suit = spades }
        if held |> Array.contains qs then me
        else
            let receiver = passTarget passDirection me
            let mustHold p =
                let played = playedBy p
                if passDirection <> hold && p = receiver then Array.append played passed |> Array.distinct
                else played
            let others = allSeats |> List.filter ((<>) me)
            let musts = others |> List.map (fun p -> p, mustHold p) |> Map
            let taken = Collections.Generic.HashSet<int>(held |> Seq.map (fun c -> c.Index))
            for p in others do
                for c in musts[p] do taken.Add(c.Index) |> ignore
            let fillers = Collections.Generic.Queue<int>([ 0 .. 51 ] |> List.filter (taken.Contains >> not))
            others
                |> List.tryPick (fun p ->
                    let cards = ResizeArray(musts[p] |> Seq.map (fun c -> c.Index))
                    while cards.Count < 13 do cards.Add(fillers.Dequeue())
                    if cards.Contains qs.Index then Some p else None)
                |> Option.defaultValue none

    let cardsHigherThan start s =
        seq { start + 1 .. ace } |> Seq.filter (fun r -> not cardsPlayed[r, s]) |> Seq.length

    let imScoreOf p = if p < 0 then 0 else imScore[p]

    // --- inference updates ---

    /// Records that the current player does not hold the given ranks, as the other seats infer.
    let updatePubInfoOnPlayer code lo hi s =
        for r in lo .. hi do
            if r = queen && s = spades && lo <> hi then ()      // the inference does not apply to Q♠
            elif Info.certainty publicInfo[player, r, s] <= Info.certainty code then
                publicInfo[player, r, s] <- code

    /// Records that every other seat infers seat p holds (or does not hold) the card, unless what is
    /// already known is at least as certain.
    let updateOthersPublicInfoForCard code p r s =
        let certainty = Info.certainty code
        if code >= 1024 then
            let alreadyKnown =
                allSeats |> List.exists (fun i ->
                    publicInfo[i, r, s] >= 1024 && Info.certainty publicInfo[i, r, s] >= certainty)
            if not alreadyKnown then
                publicInfo[p, r, s] <- code
                for i in allSeats do
                    if i <> p && Info.certainty publicInfo[i, r, s] < certainty then
                        publicInfo[i, r, s] <- 512 ||| certainty
        elif certainty > Info.certainty publicInfo[p, r, s] then
            if publicInfo[p, r, s] >= 1024 then
                for i in allSeats do
                    if i <> p && Info.certainty publicInfo[i, r, s] <= certainty then
                        publicInfo[i, r, s] <- 0
            publicInfo[p, r, s] <- code

    /// Records this seat's private inference that seat p lacks the given ranks.
    let updatePrivateInfo code owner p lo hi s =
        if owner = me then
            for r in lo .. hi do
                if Info.certainty privateInfo[p, r, s] <= Info.certainty code then
                    privateInfo[p, r, s] <- code

    let pubPlayerVoid p s =
        isVoid[p, s] || seq { 2 .. ace } |> Seq.forall (fun r -> not (Info.mayHold publicInfo[p, r, s]))

    /// Every seat now suspects the trick leader of Shooting.
    let updateArcShooter () =
        for p in allSeats do
            if p <> trickLeader then
                arcShooter[p] <- trickLeader
                arcPossOppShoot[p] <- true
                arcProbOppShoot[p] <- true

    let computePublicParameters () =
        let voidFollowers =
            [ position + 1 .. 3 ]
                |> List.filter (fun i -> pubPlayerVoid ((player + i - position) % 4) suitLed)
                |> List.length
        pubFollowersNotVoid <- 3 - voidFollowers - position
        if position = 3 && suitLed = spades && voidInSuit[suitLed] = 3 && not qsPlayed then
            publicInfo[trickLeader, queen, spades] <- 1039
        else
            let owners = allSeats |> List.filter (fun p -> Info.mayHold publicInfo[p, queen, spades])
            match owners with
                | [ owner ] -> pubQSOwner <- owner
                | _ -> pubQSOwner <- none
        pubQSOwnerPlayed <-
            if position = 0 then false
            elif qsPlayed || position = 3 then true
            elif suitLed = spades && pubFollowersNotVoid = 0 then true
            elif pubQSOwner >= 0 then playerHasPlayed[pubQSOwner]
            else
                // no later seat in the trick may hold Q♠
                [ position + 1 .. 3 ]
                    |> List.forall (fun i ->
                        not (Info.mayHold publicInfo[(player + i - position) % 4, queen, spades]))

        // whom the public believes the player targets
        let low = lowScore
        for p in allSeats do
            pubTgt[player, p] <-
                if p = player then false
                elif imScore[p] = low then true
                elif highScore <= 100 - 60 && imScore[p] < highScore && imScore[player] <= low + 10 then true
                elif imScore[player] <> low then false
                elif imScore[p] = highScore then true
                elif imScore[p] >= highScore - 10 && highScore <= 100 - 10 then true
                elif imScore[p] >= 100 - 13 then true
                else imScore[p] <= low + 10

        // whether the public believes the player helps the seat holding every point to Shoot
        let low = scoreBetweenHands[1]
        pubHelpShooter <-
            if pointsSit < 0 || player = pointsSit then false
            elif gameScore[player] = 100 - 1 then true
            elif gameScore[pointsSit] - 26 < low then false
            elif gameScore[player] > low then true
            elif scoreBetweenHands[2] - low >= 30 - pointsPlayed then false
            elif gameScore[pointsSit] = scoreBetweenHands[4] && scoreBetweenHands[4] - scoreBetweenHands[3] >= 8 then false
            else true

    let updateVoidRecordsAndOtherParameters (card : KCard) =
        let mutable newVoid = false
        if countPlayed[card.Suit] = 13 then
            for p in allSeats do isVoid[p, card.Suit] <- true
            voidInSuit[card.Suit] <- 4
        if position = 0 then
            if card.Suit = hearts && pointsPlayed = 0 then
                for s in allSuits do
                    if s <> hearts && not isVoid[player, s] then
                        isVoid[player, s] <- true
                        voidInSuit[s] <- voidInSuit[s] + 1
        elif card.Suit <> suitLed then
            sluffs <- sluffs + 1
            newVoid <- not isVoid[player, suitLed]
            if newVoid then
                voidInSuit[suitLed] <- voidInSuit[suitLed] + 1
                if voidInSuit[suitLed] < 3 || pointsPlayed > 0 || trick >= 8 then ()
                elif imScoreOf trickLeader > lowScore then updateArcShooter ()
                elif leadOver2ndLow <= 10 then updateArcShooter ()
            isVoid[player, suitLed] <- true
            if sluffPts then ()
            elif pointsInTrick = 13 || pointsInTrick = 1 then
                // the player sluffed Q♠ or a heart, so holds nothing but point cards
                for s in allSuits do
                    if s <> hearts && s <> suitLed then
                        isVoid[player, s] <- true
                        voidInSuit[s] <- 1
                if pointsInTrick = 1 then
                    for p in allSeats do
                        if p <> player then
                            isVoid[p, hearts] <- true
                            voidInSuit[hearts] <- voidInSuit[hearts] + 1
            elif pointsInTrick = 14 && card.Suit = hearts then
                for p in allSeats do
                    if p <> playerPlayedQS then
                        isVoid[p, hearts] <- true
                        voidInSuit[hearts] <- voidInSuit[hearts] + 1
        computePublicParameters ()

        // who holds the points
        if pointsInTrick = 0 then ()
        elif pointsSit = PointsSit.split then ()
        elif pubFollowersNotVoid > 0 && cardsHigherThan w suitLed > 0 then ()
        elif pointsSit = PointsSit.unplayed then pointsSit <- trickWinner
        elif pointsSit >= 0 && trickWinner <> pointsSit then pointsSit <- PointsSit.split

        if position = 0 then
            cardLed <- card.Rank
            if card.Suit = spades then
                hasLedSpades[player] <- 2
                pNotLeadingS <- none
            elif hasLedSpades[player] = 0 then
                hasLedSpades[player] <- 1
                pNotLeadingS <- player
        elif card.Suit = suitLed then
            if suitLed = spades && card.Rank >= king && card.Rank = w && not pubQSOwnerPlayed then
                playedAKS[player] <- true
        else
            hasSluffed[player] <- true
            if card.Suit = spades && card.Rank >= king then hasSluffedAKS[player] <- true

        // Q♠'s owner learns that a seat leading spades is probably the only other one with spades
        if position = 3 && suitLed = spades && not qsPlayed && trickWinner <> ownsQS && trickWinner <> trickLeader
            && w >= king && newVoid then
            possibleQSPoison <- trickWinner

        // public inferences, none of them from the first trick, which the two of clubs leads
        if trick = 1 then ()
        elif pointsSit >= 0 && pointsPlayed >= 7 then
            // the public concludes that the seat holding every point is Shooting
            if player = pointsSit || position = 0 then ()
            elif card.Suit = suitLed then
                if pubHelpShooter && trickWinner = pointsSit then
                    updatePubInfoOnPlayer 522 (card.Rank + 1) (w - 1) suitLed
                elif not pubHelpShooter && pointsInTrick > 0 && pointsInTrick < 13 && card.Rank < w then
                    updatePubInfoOnPlayer 525 (w + 1) ace suitLed
            elif card.IsQS then ()
            elif trickWinner = pointsSit && pubHelpShooter then
                updatePubInfoOnPlayer 524 (card.Rank + 1) ace card.Suit
            elif trickWinner <> pointsSit && not pubHelpShooter then
                if pubTgt[player, trickWinner] && suitLed <> spades then
                    updateOthersPublicInfoForCard 525 player queen spades
                if suitLed <> hearts && card.Suit <> hearts then
                    updatePubInfoOnPlayer 525 2 ace hearts
        else
            if position > 0 && player = trickWinner && card.Rank = queen && suitLed = spades then
                updatePubInfoOnPlayer 526 2 ace spades
            elif position = 3 && player = trickWinner && pointsInTrick = 0 && pointsSit = PointsSit.split then
                updatePubInfoOnPlayer 521 (w + 1) ace suitLed
            elif position = 3 && suitLed <> spades && pointsPlayed = 1 && pointsInTrick = 1 && card.Rank < w
                && card.Suit = suitLed then
                updatePubInfoOnPlayer 519 (w + 1) ace suitLed
            elif position > 0 && player <> trickWinner && card.Suit = suitLed then
                if not qsPlayed && suitLed = spades then ()
                elif mode = Mode.StopShoot then updatePubInfoOnPlayer 522 2 (card.Rank - 1) suitLed
                else updatePubInfoOnPlayer 522 (card.Rank + 1) (w - 1) suitLed
            elif position = 0 || card.Suit = suitLed then ()
            elif card.Suit >= clubs || (card.Suit = spades && qsPlayed) || (card.Suit = hearts && pointsSit = PointsSit.split) then
                updatePubInfoOnPlayer 524 (card.Rank + 1) ace card.Suit

            // inferences about Q♠
            if qsPlayed then ()
            elif suitLed = spades && voidInSuit[spades] = 3 then
                updateOthersPublicInfoForCard 1039 trickWinner queen spades
            elif position = 0 then
                if suitLed = spades && w >= king then
                    updateOthersPublicInfoForCard 1034 player queen spades
                elif suitLed = spades && w < jack then
                    updatePubInfoOnPlayer 519 (w + 1) jack spades
                elif suitLed = hearts && w >= king && pointsSit <> player then
                    updatePubInfoOnPlayer 521 ace ace hearts
                    updateOthersPublicInfoForCard 1033 player queen spades
                if suitLed = spades then ()
                elif highLeadsPerSuit[suitLed] >= 2 && passDirection <> hold then
                    updateOthersPublicInfoForCard 1033 player queen spades
                elif highLeadsPerSuit[suitLed] >= 3 then
                    updateOthersPublicInfoForCard 1033 player queen spades
                elif pointsSit = PointsSit.split && w >= jack then
                    updateOthersPublicInfoForCard 1033 player queen spades
            elif card.Suit = suitLed then
                if suitLed <> spades then ()
                elif card.Rank < queen && playedAKS[player] then
                    updateOthersPublicInfoForCard 1032 player queen spades
                elif w >= king && player <> trickWinner && card.Rank <> queen && pubTgt[player, trickWinner] then
                    updateOthersPublicInfoForCard 523 player queen spades
                elif prevW = king && card.Rank = ace && prevTrickWinner >= 0 && pubTgt[player, prevTrickWinner] then
                    updateOthersPublicInfoForCard 522 player queen spades
                elif card.Rank >= king && pubFollowersNotVoid > 0 && prevW < queen then
                    if passDirection = passRight then
                        updateOthersPublicInfoForCard 1032 (previousSeat player) queen spades
                    else updateOthersPublicInfoForCard 1029 player queen spades
                elif card.Rank >= king && card.Rank = w && not pubQSOwnerPlayed && aksPlayed = 2 && prevW < queen then
                    updateOthersPublicInfoForCard 1026 player queen spades
            elif suitLed <> spades && not card.IsQS
                && (pubFollowersNotVoid = 0 || w > pubLowestUnplayed[suitLed])
                && (card.Rank >= king || card.Suit <> spades || pointsSit = PointsSit.split
                    || (pointsSit >= 0 && pointsSit <> player)) then
                let code =
                    if imScoreOf trickWinner - 5 <= lowScore then 524
                    elif imScore[player] = lowScore && (imScoreOf trickWinner = highScore || imScoreOf trickWinner <= lowScore + 6) then 524
                    elif card.Rank >= king && card.Suit = spades && playerSluffs[player] = 1 then 520
                    else 0
                if code > 0 then updateOthersPublicInfoForCard code player queen spades

            // delayed inferences about Q♠ from a seat that has not led spades
            if qsPlayed then ()
            elif hasLedSpades[player] <> 1 then ()
            elif suitLed = spades && card.Suit = spades && card.Rank >= queen && card.Rank > w then
                hasLedSpades[player] <- 2
            elif isVoid[player, spades] then hasLedSpades[player] <- 2
            elif suitPulls[spades] >= 2 then hasLedSpades[player] <- 2
            elif aksPlayed = 2 then updateOthersPublicInfoForCard 1027 player queen spades
            elif cardsPlayed[queen, spades] && cardsPlayed[king, spades] && not cardsPlayed[ace, spades] then
                updateOthersPublicInfoForCard 1027 player ace spades
            elif cardsPlayed[queen, spades] && cardsPlayed[ace, spades] && not cardsPlayed[king, spades] then
                updateOthersPublicInfoForCard 1027 player king spades
            if hasLedSpades[player] = 2 then pNotLeadingS <- none

            // private inference: a seat that plays A/K♠ under someone else's spade lead lacks Q♠
            if suitLed = spades && not qsPlayed && playsAKSInTrick >= 0 && playsAKSInTrick <> player
                && playsAKSInTrick <> ownsQS then
                updatePrivateInfo 522 playsAKSInTrick player queen queen spades

    let analyzeTrickSoFar (card : KCard) =
        if position = 0 then
            suitLed <- card.Suit
            suitPulls[suitLed] <- suitPulls[suitLed] + 1
            trickWinner <- player
            w <- card.Rank
            if w >= 8 && suitLed <> spades && voidInSuit[suitLed] <> 2 then
                highLeadsPerSuit[suitLed] <- highLeadsPerSuit[suitLed] + 1
        elif card.Suit <> suitLed then
            suitSluffs[player, card.Suit] <- suitSluffs[player, card.Suit] + 1
            playerSluffs[player] <- playerSluffs[player] + 1
        elif card.Rank > w then
            prevTrickWinner <- trickWinner
            prevW <- w
            w <- card.Rank
            trickWinner <- player
        if card.Suit = hearts then
            canLeadH <- true
            pointsInTrick <- pointsInTrick + 1
        elif card.Suit <> spades then ()
        elif card.Rank = queen then
            qsOnTrick <- true
            qsPlayed <- true
            playerPlayedQS <- player
            canLeadH <- true
            pointsInTrick <- pointsInTrick + 13
            if position = 0 then qsLedOnTrick <- true
            elif suitPulls[spades] = 1 && w = card.Rank then followsWithQSOnFirstPull <- player
        elif card.Rank >= king then
            aksPlayed <- aksPlayed + 1
            playsAKSInTrick <- player
            if position = 0 then ledAKS <- player
            elif position < 3 && suitPulls[spades] = 1 && w = card.Rank && prevW < queen then
                followsWithAKSOnFirstPull <- player
        updateVoidRecordsAndOtherParameters card

    let startTrick () =
        trick <- trick + 1
        cardLed <- none
        player <- trickLeader
        playsAKSInTrick <- none
        pointsInTrick <- 0
        prevW <- none
        qsLedOnTrick <- false
        qsOnTrick <- false
        sluffs <- 0
        suitLed <- none
        trickWinner <- none
        w <- none
        Array.fill playerHasPlayed 0 4 false
        position <- 0

    let endTrick () =
        scoreInHand[trickWinner] <- scoreInHand[trickWinner] + pointsInTrick
        pointsPlayed <- pointsPlayed + pointsInTrick
        pointsUnplayed <- pointsUnplayed - pointsInTrick
        for p in allSeats do scoreInGame[p] <- gameScore[p] + scoreInHand[p]
        lowScore <- Array.min scoreInGame
        trickLeader <- trickWinner
        tricksWonBy[trickWinner] <- tricksWonBy[trickWinner] + 1
        sluffPts <- true
        if trick < 13 then startTrick ()

    do
        for c in held do hand[c.Rank, c.Suit] <- true

        // what this seat knows from the deal and the pass
        let dealt = held |> Array.filter (fun c -> not (Array.contains c received)) |> Array.append passed
        for c in dealt do
            for k in 1 .. 3 do privateInfo[(me + k) % 4, c.Rank, c.Suit] <- 0x20F
        if passDirection <> hold then
            let source = passSource passDirection me
            let target = passTarget passDirection me
            for c in received do privateInfo[source, c.Rank, c.Suit] <- 559   // the passer no longer has it
            for c in passed do privateInfo[target, c.Rank, c.Suit] <- 1055    // the receiver has it
            if passDirection = passAcross then
                // a pass of three cards in one suit leaves the passer void in it
                for s in allSuits do
                    if received |> Array.filter (fun c -> c.Suit = s) |> Array.length = 3 then
                        updatePrivateInfo 519 me source 2 ace s
                // a passer near the low score passes its worst cards
                if gameScore[source] - scoreBetweenHands[1] <= 10 then
                    for c in received do
                        if c.Rank <= 5 && c.Suit <> spades then
                            updatePrivateInfo 519 me source 2 ace c.Suit       // a low card: void in its suit
                        elif c.Suit = clubs || c.Suit = diamonds then
                            updatePrivateInfo 519 me source (c.Rank + 1) ace c.Suit   // nothing higher

        trickLeader <-
            if held |> Array.exists (fun c -> c.Rank = 2 && c.Suit = clubs) then me
            else
                allSeats
                    |> List.tryFind (fun p -> playedBy p |> Array.exists (fun c -> c.Rank = 2 && c.Suit = clubs))
                    |> Option.defaultWith (fun () ->
                        if passed |> Array.exists (fun c -> c.Rank = 2 && c.Suit = clubs) then passTarget passDirection me
                        else failwith "Cannot find 2C")
        startTrick ()

    /// Plays a card for the seat whose turn it is. For this seat's own plays, record its decision
    /// first with Remember.
    member _.Play(card : KCard) =
        if player <> me then
            mode <- Mode.LieLow   // a stand-in for the other seats' modes, as Killer Hearts' one-seat table has
            if trick = 1 && position > 0 && card.Suit <> suitLed && card.Points > 0 then sluffPts <- true
        else hand[card.Rank, card.Suit] <- false
        cardsPlayed[card.Rank, card.Suit] <- true
        countPlayed[card.Suit] <- countPlayed[card.Suit] + 1
        playerHasPlayed[player] <- true
        handSize[player] <- handSize[player] - 1
        if card.Rank = pubLowestUnplayed[card.Suit] then
            pubLowestUnplayed[card.Suit] <-
                seq { card.Rank .. ace } |> Seq.tryFind (fun r -> not cardsPlayed[r, card.Suit]) |> Option.defaultValue 15
        analyzeTrickSoFar card
        player <- nextSeat player
        twoCLeads <- false
        position <- position + 1
        if position = 4 then endTrick ()

    /// Records what this seat's decision leaves behind.
    member _.Remember(memory : Memory) =
        mode <- memory.Mode
        Array.blit memory.ImScore 0 imScore 0 4
        lowScore <- memory.LowScore
        highScore <- memory.HighScore
        leadOver2ndLow <- memory.LeadOver2ndLow
        canLeadH <- memory.CanLeadH
        sluffPts <- memory.SluffPts
        arcShooter[me] <- memory.Shooter
        arcPossOppShoot[me] <- memory.PossOppShoot
        arcProbOppShoot[me] <- memory.ProbOppShoot
        shootTry <- memory.ShootTry
        for p in 0 .. 3 do
            for r in 0 .. 14 do
                for s in 0 .. 3 do privateInfo[p, r, s] <- memory.PrivateInfo[p, r, s]

    /// Under the arena's rules, playing the Q♠ doesn't break hearts, as it does in Killer Hearts: only a heart
    /// does. Call after each Q♠; the same as UnbreakHearts in the C# engine and unbreak_hearts in the arena bot.
    member _.UnbreakHearts() = if countPlayed[hearts] = 0 then canLeadH <- false

    /// How to depart from Killer Hearts, if at all. Set before replaying a deal.
    member val Options = Options.faithful with get, set
    member t.NoShoot = t.Options.NoShoot
    member t.FixQuirks = t.Options.FixQuirks

    member _.Me = me
    member _.Dealer = dealer
    member _.PassDirection = passDirection
    member _.Passed = passed
    member _.Received = received
    member _.Hand = hand
    member _.CardsPlayed = cardsPlayed
    member _.CountPlayed = countPlayed
    member _.Trick = trick
    member _.Seq = position
    member _.Player = player
    member _.TrickLeader = trickLeader
    member _.SuitLed = suitLed
    member _.W = w
    member _.TrickWinner = trickWinner
    member _.PrevW = prevW
    member _.PrevTrickWinner = prevTrickWinner
    member _.PointsInTrick = pointsInTrick
    member _.PointsPlayed = pointsPlayed
    member _.PointsUnplayed = pointsUnplayed
    member _.PointsSit = pointsSit
    member _.QSOnTrick = qsOnTrick
    member _.QSLedOnTrick = qsLedOnTrick
    member _.QSPlayed = qsPlayed
    member _.PlayerPlayedQS = playerPlayedQS
    member _.CardLed = cardLed
    member _.PlaysAKSInTrick = playsAKSInTrick
    member _.Sluffs = sluffs
    member _.AKSPlayed = aksPlayed
    member _.CanLeadH = canLeadH
    member _.SluffPts = sluffPts
    member _.TwoCLeads = twoCLeads
    member _.ScoreInHand = scoreInHand
    member _.ScoreInGame = scoreInGame
    member _.GameScore = gameScore
    member _.ScoreBetweenHands = scoreBetweenHands
    member _.TricksWonBy = tricksWonBy
    member _.ImScore = imScore
    member _.LowScore = lowScore
    member _.HighScore = highScore
    member _.LeadOver2ndLow = leadOver2ndLow
    member _.PlayerHasPlayed = playerHasPlayed
    member _.PubLowestUnplayed = pubLowestUnplayed
    member _.HandSize = handSize
    member _.IsVoid = isVoid
    member _.VoidInSuit = voidInSuit
    member _.SuitPulls = suitPulls
    member _.HighLeadsPerSuit = highLeadsPerSuit
    member _.SuitSluffs = suitSluffs
    member _.PlayerSluffs = playerSluffs
    member _.HasLedSpades = hasLedSpades
    member _.PNotLeadingS = pNotLeadingS
    member _.PlayedAKS = playedAKS
    member _.HasSluffed = hasSluffed
    member _.HasSluffedAKS = hasSluffedAKS
    member _.LedAKS = ledAKS
    member _.FollowsWithAKSOnFirstPull = followsWithAKSOnFirstPull
    member _.FollowsWithQSOnFirstPull = followsWithQSOnFirstPull
    member _.PossibleQSPoison = possibleQSPoison
    member _.OwnsQS = ownsQS
    member _.PublicInfo = publicInfo
    member _.PrivateInfo = privateInfo
    member _.PubFollowersNotVoid = pubFollowersNotVoid
    member _.PubQSOwner = pubQSOwner
    member _.PubQSOwnerPlayed = pubQSOwnerPlayed
    member _.PubTgt = pubTgt
    member _.PubHelpShooter = pubHelpShooter
    member _.ArcShooter = arcShooter
    member _.ArcPossOppShoot = arcPossOppShoot
    member _.ArcProbOppShoot = arcProbOppShoot
    member _.Mode = mode
    member _.ShootTry = shootTry
