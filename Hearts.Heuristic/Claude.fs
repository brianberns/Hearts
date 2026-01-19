namespace Hearts.Heuristic

open PlayingCards
open Hearts

module Claude =

    // ============================================================
    // Constants
    // ============================================================

    let private queenOfSpades = Card.create Rank.Queen Suit.Spades

    /// Minimum points to commit to shooting during play
    let private shootingCommitThreshold = 14

    /// Minimum points to detect opponent shooting attempt
    let private antiShootingThreshold = 18

    /// Minimum low spades needed to protect QS
    let private qsProtectionThreshold = 3

    /// Maximum suit length to consider for voiding during pass
    let private maxVoidCandidateLength = 3

    // ============================================================
    // Card Helpers
    // ============================================================

    let private isQS (card : Card) =
        card.Suit = Suit.Spades && card.Rank = Rank.Queen

    let private bySuit (suit : Suit) (cards : Card[]) =
        cards |> Array.filter (fun c -> c.Suit = suit)

    let private byMinRank (minRank : Rank) (cards : Card[]) =
        cards |> Array.filter (fun c -> c.Rank >= minRank)

    let private belowRank (rank : Rank) (cards : Card[]) =
        cards |> Array.filter (fun c -> c.Rank < rank)

    let private aboveRank (rank : Rank) (cards : Card[]) =
        cards |> Array.filter (fun c -> c.Rank > rank)

    let private excludingQS (cards : Card[]) =
        cards |> Array.filter (fun c -> not (isQS c))

    let private highest (cards : Card[]) =
        cards |> Array.maxBy (fun c -> c.Rank)

    let private lowest (cards : Card[]) =
        cards |> Array.minBy (fun c -> c.Rank)

    let private anyOpponentVoid (suit : Suit) (mySeat : Seat) (voids : Set<Seat * Suit>) =
        voids |> Set.exists (fun (seat, s) -> seat <> mySeat && s = suit)

    /// Get highest card, preferring non-QS if available
    let private highestPreferNonQS (cards : Card[]) =
        let nonQS = excludingQS cards
        if nonQS.Length > 0 then highest nonQS else highest cards

    /// Get lowest card, preferring non-QS if available
    let private lowestPreferNonQS (cards : Card[]) =
        let nonQS = excludingQS cards
        if nonQS.Length > 0 then lowest nonQS else lowest cards

    /// Try to safely dump QS when spades led and someone played higher
    let private trySafeQSDump (cardsInSuit : Card[]) (highRank : Rank option) (suitLed : Suit) =
        match cardsInSuit |> Array.tryFind isQS, highRank with
        | Some qs, Some hr when suitLed = Suit.Spades && hr > Rank.Queen -> Some qs
        | _ -> None

    /// Try to win trick with minimum winning card
    let private tryWinTrick (cards : Card[]) (highRank : Rank option) =
        match highRank with
        | Some hr ->
            let winners = cards |> aboveRank hr
            if winners.Length > 0 then Some (lowest winners) else None
        | None -> None

    // ============================================================
    // Shooting Detection
    // ============================================================

    /// Detect if we should commit to shooting (during play)
    let private isAttemptingToShoot (deal : ClosedDeal) mySeat =
        let myPoints = deal.Score[mySeat]
        let totalPointsTaken = deal.Score |> Score.sum
        myPoints > 0 && myPoints = totalPointsTaken && myPoints >= shootingCommitThreshold

    /// Detect if initial hand is strong enough to attempt shooting (during passing)
    let private isShootingHand (hand : Hand) =
        let cards = hand |> Seq.toList
        let hearts = cards |> List.filter (fun c -> c.Suit = Suit.Hearts)
        let spades = cards |> List.filter (fun c -> c.Suit = Suit.Spades)

        let aces = cards |> List.filter (fun c -> c.Rank = Rank.Ace) |> List.length
        let kings = cards |> List.filter (fun c -> c.Rank = Rank.King) |> List.length

        let hasQS = spades |> List.exists isQS
        let hasAceSpades = spades |> List.exists (fun c -> c.Rank = Rank.Ace)
        let hasKingSpades = spades |> List.exists (fun c -> c.Rank = Rank.King)
        let qsProtected = hasQS && (hasAceSpades || hasKingSpades)

        let highHearts = hearts |> List.filter (fun c -> c.Rank >= Rank.Ten) |> List.length

        (qsProtected && aces >= 2 && highHearts >= 3) ||
        (aces >= 3 && kings >= 2 && highHearts >= 2) ||
        (hasQS && aces >= 3 && (aces + kings) >= 5)

    /// Detect if an opponent is attempting to shoot
    let private detectShootingOpponent (deal : ClosedDeal) mySeat =
        let scores = Score.indexed deal.Score
        let (potentialShooter, shooterPoints) = scores |> Seq.maxBy snd
        let totalPointsTaken = deal.Score |> Score.sum
        let isRisk =
            potentialShooter <> mySeat &&
            shooterPoints > 0 &&
            shooterPoints = totalPointsTaken &&
            shooterPoints >= antiShootingThreshold
        if isRisk then Some potentialShooter else None

    // ============================================================
    // Passing Strategy
    // ============================================================

    let private choosePassOffensive (legalCards : Card[]) =
        let byRank = legalCards |> Array.sortBy (fun c -> c.Rank)
        let lowMinorCards =
            byRank |> Array.filter (fun c -> c.Suit <> Suit.Hearts && c.Suit <> Suit.Spades)
        if lowMinorCards.Length > 0 then lowMinorCards.[0] else byRank.[0]

    let private choosePassDefensive (hand : Hand) (legalCards : Card[]) =
        let spades = hand |> Seq.filter (fun c -> c.Suit = Suit.Spades) |> Seq.toList
        let hearts = hand |> Seq.filter (fun c -> c.Suit = Suit.Hearts) |> Seq.toList
        let clubs = hand |> Seq.filter (fun c -> c.Suit = Suit.Clubs) |> Seq.toList
        let diamonds = hand |> Seq.filter (fun c -> c.Suit = Suit.Diamonds) |> Seq.toList

        let hasQS = spades |> List.exists isQS
        let lowSpades = spades |> List.filter (fun c -> c.Rank < Rank.Queen) |> List.length
        let highSpades = spades |> List.filter (fun c -> c.Rank > Rank.Queen)
        let hasHighSpadeProtection = highSpades.Length >= 2 || (highSpades.Length >= 1 && lowSpades >= 2)
        let aceHeart = hearts |> List.filter (fun c -> c.Rank = Rank.Ace)
        let highHearts = hearts |> List.filter (fun c -> c.Rank >= Rank.Queen)
                                |> List.sortByDescending (fun c -> c.Rank)

        let voidCandidates =
            [clubs; diamonds]
            |> List.filter (fun suit -> suit.Length > 0 && suit.Length <= maxVoidCandidateLength)
            |> List.sortBy List.length
            |> List.collect id
            |> List.sortByDescending (fun c -> c.Rank)

        let qsUnprotected = lowSpades < qsProtectionThreshold && not hasHighSpadeProtection

        let candidates =
            [
                if hasQS && qsUnprotected then [queenOfSpades] else []
                if highSpades.Length > 0 then highSpades |> List.sortByDescending (fun c -> c.Rank) else []
                aceHeart
                highHearts
                voidCandidates
                legalCards |> Array.filter (fun c -> c.Rank >= Rank.Jack)
                           |> Array.sortByDescending (fun c -> c.Rank)
                           |> Array.toList
            ]
            |> List.concat
            |> List.distinct

        candidates
        |> List.tryFind (fun c -> legalCards |> Array.contains c)
        |> Option.defaultValue (highest legalCards)

    let private choosePass (hand : Hand) (legalCards : Card[]) =
        if isShootingHand hand then choosePassOffensive legalCards
        else choosePassDefensive hand legalCards

    // ============================================================
    // Leading Strategy
    // ============================================================

    let private leadFromShortestSafe (cards : Card[]) (mySeat : Seat) (voids : Set<Seat * Suit>) =
        let grouped =
            cards
            |> Array.groupBy (fun c -> c.Suit)
            |> Array.sortBy (fun (_, suitCards) -> suitCards.Length)

        let isSingletonHigh (suitCards : Card[]) =
            suitCards.Length = 1 && (lowest suitCards).Rank >= Rank.King

        let isSafeLead (suit, suitCards) =
            not (isSingletonHigh suitCards) && not (anyOpponentVoid suit mySeat voids)

        match grouped |> Array.tryFind isSafeLead with
        | Some (_, suitCards) -> lowest suitCards
        | None ->
            match grouped |> Array.tryFind (fun (_, sc) -> not (isSingletonHigh sc)) with
            | Some (_, suitCards) -> lowest suitCards
            | None -> grouped.[0] |> snd |> lowest

    let private chooseLeadShooting (legalCards : Card[]) =
        let aces = legalCards |> Array.filter (fun c -> c.Rank = Rank.Ace)
        if aces.Length > 0 then
            aces |> Array.tryFind (fun c -> c.Suit = Suit.Hearts)
                 |> Option.defaultValue aces.[0]
        else
            highest legalCards

    let private chooseLead (hand : Hand) (deal : ClosedDeal) (legalCards : Card[]) (mySeat : Seat) =
        if isAttemptingToShoot deal mySeat then
            chooseLeadShooting legalCards
        else
            let voids = deal.Voids
            let qsPlayed = not (deal.UnplayedCards.Contains(queenOfSpades))
            let holdingQS = hand.Contains(queenOfSpades)
            let hasHighSpades = hand |> Seq.exists (fun c -> c.Suit = Suit.Spades && c.Rank > Rank.Queen)
            let lowSpades = legalCards |> bySuit Suit.Spades |> belowRank Rank.Queen

            if not qsPlayed then
                if holdingQS || hasHighSpades then
                    let nonSpades = legalCards |> Array.filter (fun c -> c.Suit <> Suit.Spades)
                    if nonSpades.Length > 0 then leadFromShortestSafe nonSpades mySeat voids
                    else lowest legalCards
                else
                    if lowSpades.Length > 0 then lowest lowSpades
                    else leadFromShortestSafe legalCards mySeat voids
            else
                let nonQS = excludingQS legalCards
                if nonQS.Length > 0 then leadFromShortestSafe nonQS mySeat voids
                else leadFromShortestSafe legalCards mySeat voids

    // ============================================================
    // Following Strategy
    // ============================================================

    let private chooseFollowShooting (cardsInSuit : Card[]) (legalCards : Card[]) (highRank : Rank option) =
        if cardsInSuit.Length > 0 then
            match tryWinTrick cardsInSuit highRank with
            | Some card -> card
            | None ->
                match highRank with
                | Some _ -> lowest cardsInSuit
                | None -> highest cardsInSuit
        else
            lowest legalCards

    let private chooseFollowLastToPlay (cardsInSuit : Card[]) (highRank : Rank option) (suitLed : Suit) (trickPoints : int) (shouldIntercept : bool) =
        match trySafeQSDump cardsInSuit highRank suitLed with
        | Some qs -> qs
        | None ->
            if shouldIntercept then
                tryWinTrick cardsInSuit highRank
                |> Option.defaultWith (fun () -> highestPreferNonQS cardsInSuit)
            elif trickPoints <= 0 then
                highestPreferNonQS cardsInSuit
            else
                match highRank with
                | Some hr ->
                    let belowWinner = cardsInSuit |> belowRank hr
                    if belowWinner.Length > 0 then highest belowWinner
                    else highestPreferNonQS cardsInSuit
                | None ->
                    lowest cardsInSuit

    let private chooseFollowNotLast (cardsInSuit : Card[]) (highRank : Rank option) (suitLed : Suit) (qsPlayed : bool) (shouldIntercept : bool) =
        if shouldIntercept then
            tryWinTrick cardsInSuit highRank
            |> Option.defaultWith (fun () -> highestPreferNonQS cardsInSuit)
        else
            match trySafeQSDump cardsInSuit highRank suitLed with
            | Some qs -> qs
            | None ->
                match highRank with
                | Some hr ->
                    let belowWinner = cardsInSuit |> belowRank hr
                    if suitLed = Suit.Spades && not qsPlayed then
                        let belowQueen = cardsInSuit |> belowRank Rank.Queen
                        if belowQueen.Length > 0 then highest belowQueen
                        elif belowWinner.Length > 0 then highest belowWinner
                        else lowestPreferNonQS cardsInSuit
                    elif belowWinner.Length > 0 then
                        highest belowWinner
                    else
                        lowestPreferNonQS cardsInSuit
                | None ->
                    lowest cardsInSuit

    let private tryDumpHighestHeart (legalCards : Card[]) =
        let hearts = legalCards |> bySuit Suit.Hearts
        if hearts.Length > 0 then Some (highest hearts) else None

    let private dumpHighestFromShortestSuit (legalCards : Card[]) (holdingQS : bool) (hand : Hand) =
        let excludedSuits =
            if holdingQS then set [Suit.Hearts; Suit.Spades]
            else set [Suit.Hearts]
        let eligible = legalCards |> Array.filter (fun c -> not (excludedSuits.Contains c.Suit))
        if eligible.Length > 0 then
            let bySuitLength =
                eligible
                |> Array.groupBy (fun c -> c.Suit)
                |> Array.map (fun (suit, cards) ->
                    let handCount = hand |> Seq.filter (fun c -> c.Suit = suit) |> Seq.length
                    (cards, handCount))
                |> Array.sortBy snd
            fst bySuitLength.[0] |> highest
        else
            highest legalCards

    let private chooseDiscard (legalCards : Card[]) (qsPlayed : bool) (holdingQS : bool) (hand : Hand) =
        match legalCards |> Array.tryFind isQS with
        | Some qs -> qs
        | None ->
            if not qsPlayed && not holdingQS then
                let highSpades = legalCards |> bySuit Suit.Spades |> aboveRank Rank.Queen
                if highSpades.Length > 0 then
                    highest highSpades
                else
                    let midSpades = legalCards |> bySuit Suit.Spades
                                               |> byMinRank Rank.Ten
                                               |> belowRank Rank.Queen
                    if midSpades.Length > 0 then highest midSpades
                    else tryDumpHighestHeart legalCards
                         |> Option.defaultWith (fun () -> dumpHighestFromShortestSuit legalCards holdingQS hand)
            else
                tryDumpHighestHeart legalCards
                |> Option.defaultWith (fun () -> dumpHighestFromShortestSuit legalCards holdingQS hand)

    let private chooseFollow (hand : Hand) (deal : ClosedDeal) (trick : Trick) (legalCards : Card[]) (mySeat : Seat) =
        let suitLed = trick.SuitLedOpt |> Option.get
        let isLastToPlay = trick.Cards.Length = 3
        let isFirstTrick = deal.CompletedTricks.IsEmpty

        let qsPlayed = not (deal.UnplayedCards.Contains(queenOfSpades))
        let holdingQS = hand.Contains(queenOfSpades)

        let trickPoints = Trick.pointValue trick
        let highRank = trick.HighPlayOpt |> Option.map (fun (_, c) -> c.Rank)

        let cardsInSuit = legalCards |> bySuit suitLed

        if isAttemptingToShoot deal mySeat then
            chooseFollowShooting cardsInSuit legalCards highRank
        else
            let shooterOpt = detectShootingOpponent deal mySeat
            let shooterIsWinning =
                match shooterOpt, trick.HighPlayOpt with
                | Some shooter, Some (winningSeat, _) -> shooter = winningSeat
                | _ -> false
            let shouldIntercept = shooterOpt.IsSome && shooterIsWinning

            if cardsInSuit.Length > 0 then
                if isFirstTrick then highest cardsInSuit
                elif isLastToPlay then chooseFollowLastToPlay cardsInSuit highRank suitLed trickPoints shouldIntercept
                else chooseFollowNotLast cardsInSuit highRank suitLed qsPlayed shouldIntercept
            else
                chooseDiscard legalCards qsPlayed holdingQS hand

    // ============================================================
    // Player Entry Point
    // ============================================================

    let player =
        let act (infoSet : InformationSet) =
            if infoSet.LegalActions.Length = 1 then
                infoSet.LegalActions.[0]
            else
                match infoSet.LegalActionType with
                | ActionType.Pass ->
                    choosePass infoSet.Hand infoSet.LegalActions
                | ActionType.Play ->
                    match infoSet.Deal.CurrentTrickOpt with
                    | None -> failwith "No current trick"
                    | Some trick ->
                        if trick.Cards.IsEmpty then
                            chooseLead infoSet.Hand infoSet.Deal infoSet.LegalActions infoSet.Player
                        else
                            chooseFollow infoSet.Hand infoSet.Deal trick infoSet.LegalActions infoSet.Player

        { Act = act }
