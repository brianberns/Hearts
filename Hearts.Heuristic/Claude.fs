namespace Hearts.Heuristic

open PlayingCards
open Hearts

module Claude =

    // ============================================================
    // Constants and Helpers
    // ============================================================

    let private queenOfSpades = Card.create Rank.Queen Suit.Spades

    let private isQS (card : Card) =
        card.Suit = Suit.Spades && card.Rank = Rank.Queen

    let private anyOpponentVoid (suit : Suit) (mySeat : Seat) (voids : Set<Seat * Suit>) =
        voids |> Set.exists (fun (seat, s) -> seat <> mySeat && s = suit)

    /// Get highest card, preferring non-QS if available
    let private highestPreferNonQS (cards : Card[]) =
        let nonQS = cards |> Array.filter (fun c -> not (isQS c))
        if nonQS.Length > 0 then nonQS |> Array.maxBy (fun c -> c.Rank)
        else cards |> Array.maxBy (fun c -> c.Rank)

    /// Get lowest card, preferring non-QS if available
    let private lowestPreferNonQS (cards : Card[]) =
        let nonQS = cards |> Array.filter (fun c -> not (isQS c))
        if nonQS.Length > 0 then nonQS |> Array.minBy (fun c -> c.Rank)
        else cards |> Array.minBy (fun c -> c.Rank)

    // ============================================================
    // Shooting Detection
    // ============================================================

    /// Detect if we should commit to shooting (during play)
    let private isAttemptingToShoot (deal : ClosedDeal) (mySeat : Seat) =
        let myPoints = deal.Score[mySeat]
        let totalPointsTaken = deal.Score |> Score.sum
        myPoints > 0 && myPoints = totalPointsTaken && myPoints >= 14

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

    // ============================================================
    // Passing Strategy
    // ============================================================

    let private choosePassOffensive (legalCards : Card[]) =
        let byRank = legalCards |> Array.sortBy (fun c -> c.Rank)
        let lowMinorCards =
            byRank |> Array.filter (fun c -> c.Suit <> Suit.Hearts && c.Suit <> Suit.Spades)
        if lowMinorCards.Length > 0 then lowMinorCards.[0]
        else byRank.[0]

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
            |> List.filter (fun suitCards -> suitCards.Length > 0 && suitCards.Length <= 3)
            |> List.sortBy List.length
            |> List.collect id
            |> List.sortByDescending (fun c -> c.Rank)

        let qsUnprotected = lowSpades < 3 && not hasHighSpadeProtection

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
        |> Option.defaultValue (legalCards |> Array.maxBy (fun c -> c.Rank))

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

        let isSafeLead (suit : Suit, suitCards : Card[]) =
            let lowest = suitCards |> Array.minBy (fun c -> c.Rank)
            not (suitCards.Length = 1 && lowest.Rank >= Rank.King) &&
            not (anyOpponentVoid suit mySeat voids)

        match grouped |> Array.tryFind isSafeLead with
        | Some (_, suitCards) -> suitCards |> Array.minBy (fun c -> c.Rank)
        | None ->
            let notSingletonHigh =
                grouped |> Array.tryFind (fun (_, suitCards) ->
                    let lowest = suitCards |> Array.minBy (fun c -> c.Rank)
                    not (suitCards.Length = 1 && lowest.Rank >= Rank.King))
            match notSingletonHigh with
            | Some (_, suitCards) -> suitCards |> Array.minBy (fun c -> c.Rank)
            | None -> grouped.[0] |> snd |> Array.minBy (fun c -> c.Rank)

    let private chooseLeadShooting (legalCards : Card[]) =
        let aces = legalCards |> Array.filter (fun c -> c.Rank = Rank.Ace)
        if aces.Length > 0 then
            aces |> Array.tryFind (fun c -> c.Suit = Suit.Hearts)
                 |> Option.defaultValue aces.[0]
        else
            legalCards |> Array.maxBy (fun c -> c.Rank)

    let private chooseLead (hand : Hand) (deal : ClosedDeal) (legalCards : Card[]) (mySeat : Seat) =
        if isAttemptingToShoot deal mySeat then
            chooseLeadShooting legalCards
        else
            let voids = deal.Voids
            let qsPlayed = not (deal.UnplayedCards.Contains(queenOfSpades))
            let holdingQS = hand.Contains(queenOfSpades)
            let hasHighSpades = hand |> Seq.exists (fun c -> c.Suit = Suit.Spades && c.Rank > Rank.Queen)
            let lowSpades = legalCards |> Array.filter (fun c -> c.Suit = Suit.Spades && c.Rank < Rank.Queen)

            if not qsPlayed then
                if holdingQS || hasHighSpades then
                    let nonSpades = legalCards |> Array.filter (fun c -> c.Suit <> Suit.Spades)
                    if nonSpades.Length > 0 then leadFromShortestSafe nonSpades mySeat voids
                    else legalCards |> Array.minBy (fun c -> c.Rank)
                else
                    if lowSpades.Length > 0 then lowSpades |> Array.minBy (fun c -> c.Rank)
                    else leadFromShortestSafe legalCards mySeat voids
            else
                let nonQS = legalCards |> Array.filter (fun c -> not (isQS c))
                if nonQS.Length > 0 then leadFromShortestSafe nonQS mySeat voids
                else leadFromShortestSafe legalCards mySeat voids

    // ============================================================
    // Following Strategy
    // ============================================================

    let private chooseFollowShooting (cardsInSuit : Card[]) (legalCards : Card[]) (highRank : Rank option) =
        if cardsInSuit.Length > 0 then
            match highRank with
            | Some hr ->
                let winningCards = cardsInSuit |> Array.filter (fun c -> c.Rank > hr)
                if winningCards.Length > 0 then winningCards |> Array.minBy (fun c -> c.Rank)
                else cardsInSuit |> Array.minBy (fun c -> c.Rank)
            | None ->
                cardsInSuit |> Array.maxBy (fun c -> c.Rank)
        else
            legalCards |> Array.minBy (fun c -> c.Rank)

    let private tryIntercept (cardsInSuit : Card[]) (highRank : Rank option) =
        match highRank with
        | Some hr ->
            let winningCards = cardsInSuit |> Array.filter (fun c -> c.Rank > hr)
            if winningCards.Length > 0 then Some (winningCards |> Array.minBy (fun c -> c.Rank))
            else None
        | None -> None

    let private chooseFollowLastToPlay
        (cardsInSuit : Card[])
        (highRank : Rank option)
        (suitLed : Suit)
        (trickPoints : int)
        (shouldIntercept : bool) =

        match cardsInSuit |> Array.tryFind isQS, highRank with
        | Some qs, Some hr when suitLed = Suit.Spades && hr > Rank.Queen -> qs
        | _ ->
            if shouldIntercept then
                tryIntercept cardsInSuit highRank
                |> Option.defaultWith (fun () -> highestPreferNonQS cardsInSuit)
            elif trickPoints <= 0 then
                highestPreferNonQS cardsInSuit
            else
                match highRank with
                | Some hr ->
                    let belowWinner = cardsInSuit |> Array.filter (fun c -> c.Rank < hr)
                    if belowWinner.Length > 0 then belowWinner |> Array.maxBy (fun c -> c.Rank)
                    else highestPreferNonQS cardsInSuit
                | None ->
                    cardsInSuit |> Array.minBy (fun c -> c.Rank)

    let private chooseFollowNotLast
        (cardsInSuit : Card[])
        (highRank : Rank option)
        (suitLed : Suit)
        (qsPlayed : bool)
        (shouldIntercept : bool) =

        if shouldIntercept then
            match tryIntercept cardsInSuit highRank with
            | Some card -> card
            | None -> highestPreferNonQS cardsInSuit
        else
            match cardsInSuit |> Array.tryFind isQS, highRank with
            | Some qs, Some hr when suitLed = Suit.Spades && hr > Rank.Queen -> qs
            | _ ->
                match highRank with
                | Some hr ->
                    let belowWinner = cardsInSuit |> Array.filter (fun c -> c.Rank < hr)
                    if suitLed = Suit.Spades && not qsPlayed then
                        let belowQueen = cardsInSuit |> Array.filter (fun c -> c.Rank < Rank.Queen)
                        if belowQueen.Length > 0 then belowQueen |> Array.maxBy (fun c -> c.Rank)
                        elif belowWinner.Length > 0 then belowWinner |> Array.maxBy (fun c -> c.Rank)
                        else lowestPreferNonQS cardsInSuit
                    elif belowWinner.Length > 0 then
                        belowWinner |> Array.maxBy (fun c -> c.Rank)
                    else
                        lowestPreferNonQS cardsInSuit
                | None ->
                    cardsInSuit |> Array.minBy (fun c -> c.Rank)

    let private chooseDiscard (legalCards : Card[]) (qsPlayed : bool) (holdingQS : bool) (hand : Hand) =
        match legalCards |> Array.tryFind isQS with
        | Some qs -> qs
        | None ->
            let tryDumpHighestHeart () =
                let hearts = legalCards |> Array.filter (fun c -> c.Suit = Suit.Hearts)
                if hearts.Length > 0 then Some (hearts |> Array.maxBy (fun c -> c.Rank))
                else None

            let dumpHighestNonHeart () =
                let excludedSuits =
                    if holdingQS then set [Suit.Hearts; Suit.Spades]
                    else set [Suit.Hearts]
                let nonHearts = legalCards |> Array.filter (fun c -> not (excludedSuits.Contains c.Suit))
                if nonHearts.Length > 0 then
                    let bySuit =
                        nonHearts
                        |> Array.groupBy (fun c -> c.Suit)
                        |> Array.map (fun (suit, cards) ->
                            let handSuitCount = hand |> Seq.filter (fun c -> c.Suit = suit) |> Seq.length
                            (cards, handSuitCount))
                        |> Array.sortBy snd
                    fst bySuit.[0] |> Array.maxBy (fun c -> c.Rank)
                else
                    legalCards |> Array.maxBy (fun c -> c.Rank)

            if not qsPlayed && not holdingQS then
                let highSpades = legalCards |> Array.filter (fun c -> c.Suit = Suit.Spades && c.Rank > Rank.Queen)
                if highSpades.Length > 0 then
                    highSpades |> Array.maxBy (fun c -> c.Rank)
                else
                    let midSpades = legalCards |> Array.filter (fun c -> c.Suit = Suit.Spades && c.Rank >= Rank.Ten && c.Rank < Rank.Queen)
                    if midSpades.Length > 0 then midSpades |> Array.maxBy (fun c -> c.Rank)
                    else tryDumpHighestHeart () |> Option.defaultWith dumpHighestNonHeart
            else
                tryDumpHighestHeart () |> Option.defaultWith dumpHighestNonHeart

    let private chooseFollow (hand : Hand) (deal : ClosedDeal) (trick : Trick) (legalCards : Card[]) (mySeat : Seat) =
        let suitLed = trick.SuitLedOpt |> Option.get
        let isLastToPlay = trick.Cards.Length = 3
        let isFirstTrick = deal.CompletedTricks.IsEmpty

        let qsPlayed = not (deal.UnplayedCards.Contains(queenOfSpades))
        let holdingQS = hand.Contains(queenOfSpades)

        let trickPoints = Trick.pointValue trick
        let highRank = trick.HighPlayOpt |> Option.map (fun (_, c) -> c.Rank)

        let cardsInSuit = legalCards |> Array.filter (fun c -> c.Suit = suitLed)

        if isAttemptingToShoot deal mySeat then
            chooseFollowShooting cardsInSuit legalCards highRank
        else
            let scores = Score.indexed deal.Score
            let (potentialShooter, shooterPoints) = scores |> Seq.maxBy snd
            let totalPointsTaken = deal.Score |> Score.sum
            let isShootingRisk =
                potentialShooter <> mySeat &&
                shooterPoints > 0 &&
                shooterPoints = totalPointsTaken &&
                shooterPoints >= 18
            let shooterIsWinning =
                trick.HighPlayOpt |> Option.map fst |> Option.contains potentialShooter
            let shouldIntercept = isShootingRisk && shooterIsWinning

            if cardsInSuit.Length > 0 then
                if isFirstTrick then
                    cardsInSuit |> Array.maxBy (fun c -> c.Rank)
                elif isLastToPlay then
                    chooseFollowLastToPlay cardsInSuit highRank suitLed trickPoints shouldIntercept
                else
                    chooseFollowNotLast cardsInSuit highRank suitLed qsPlayed shouldIntercept
            else
                chooseDiscard legalCards qsPlayed holdingQS hand

    // ============================================================
    // Player Entry Point
    // ============================================================

    let player =
        let act (infoSet : InformationSet) =
            let legalActions = infoSet.LegalActions

            if legalActions.Length = 1 then
                legalActions.[0]
            else
                match infoSet.LegalActionType with
                | ActionType.Pass ->
                    choosePass infoSet.Hand legalActions
                | ActionType.Play ->
                    match infoSet.Deal.CurrentTrickOpt with
                    | None -> failwith "No current trick"
                    | Some trick ->
                        if trick.Cards.IsEmpty then
                            chooseLead infoSet.Hand infoSet.Deal legalActions infoSet.Player
                        else
                            chooseFollow infoSet.Hand infoSet.Deal trick legalActions infoSet.Player

        { Act = act }
