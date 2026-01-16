namespace Hearts.Heuristic

open PlayingCards
open Hearts

module Claude =

    // ============================================================
    // Constants
    // ============================================================

    let private queenOfSpades = Card.create Rank.Queen Suit.Spades

    // ============================================================
    // Card Analysis Helpers
    // ============================================================

    let private isQS (card : Card) =
        card.Suit = Suit.Spades && card.Rank = Rank.Queen

    let private isHighestRemaining (card : Card) (unplayedCards : Set<Card>) =
        unplayedCards
        |> Set.exists (fun c -> c.Suit = card.Suit && c.Rank > card.Rank)
        |> not

    let private anyOpponentVoid (suit : Suit) (mySeat : Seat) (voids : Set<Seat * Suit>) =
        voids |> Set.exists (fun (seat, s) -> seat <> mySeat && s = suit)

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
        if lowMinorCards.Length > 0 then
            lowMinorCards.[0]
        else
            byRank.[0]

    let private choosePassDefensive (hand : Hand) (legalCards : Card[]) =
        let spades = hand |> Seq.filter (fun c -> c.Suit = Suit.Spades) |> Seq.toList
        let hearts = hand |> Seq.filter (fun c -> c.Suit = Suit.Hearts) |> Seq.toList
        let clubs = hand |> Seq.filter (fun c -> c.Suit = Suit.Clubs) |> Seq.toList
        let diamonds = hand |> Seq.filter (fun c -> c.Suit = Suit.Diamonds) |> Seq.toList

        let hasQS = spades |> List.exists isQS
        let lowSpades = spades |> List.filter (fun c -> c.Rank < Rank.Queen) |> List.length
        let highSpades = spades |> List.filter (fun c -> c.Rank > Rank.Queen)
        let highHearts = hearts |> List.filter (fun c -> c.Rank >= Rank.Queen)
                                |> List.sortByDescending (fun c -> c.Rank)

        let voidCandidates =
            [clubs; diamonds]
            |> List.filter (fun suitCards -> suitCards.Length > 0 && suitCards.Length <= 4)
            |> List.sortBy (fun suitCards -> suitCards.Length)
            |> List.collect id
            |> List.sortByDescending (fun c -> c.Rank)

        let candidates =
            [
                if hasQS && lowSpades < 3 then [queenOfSpades] else []
                if highSpades.Length > 0 then highSpades |> List.sortByDescending (fun c -> c.Rank) else []
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
        if isShootingHand hand then
            choosePassOffensive legalCards
        else
            choosePassDefensive hand legalCards

    // ============================================================
    // Leading Strategy
    // ============================================================

    let private leadFromShortestSafe (cards: Card[]) (mySeat : Seat) (voids : Set<Seat * Suit>) =
        let grouped =
            cards
            |> Array.groupBy (fun c -> c.Suit)
            |> Array.sortBy (fun (_, suitCards) -> suitCards.Length)

        let isSafeLead (suit : Suit, suitCards : Card[]) =
            let lowest = suitCards |> Array.minBy (fun c -> c.Rank)
            not (suitCards.Length = 1 && lowest.Rank >= Rank.King) &&
            not (anyOpponentVoid suit mySeat voids)

        let safeLead = grouped |> Array.tryFind isSafeLead

        match safeLead with
        | Some (_, suitCards) -> suitCards |> Array.minBy (fun c -> c.Rank)
        | None ->
            let fallbackLead =
                grouped
                |> Array.tryFind (fun (_, suitCards) ->
                    let lowest = suitCards |> Array.minBy (fun c -> c.Rank)
                    not (suitCards.Length = 1 && lowest.Rank >= Rank.King))
            match fallbackLead with
            | Some (_, suitCards) -> suitCards |> Array.minBy (fun c -> c.Rank)
            | None -> grouped.[0] |> snd |> Array.minBy (fun c -> c.Rank)

    let private chooseLeadShooting (legalCards : Card[]) =
        let aces = legalCards |> Array.filter (fun c -> c.Rank = Rank.Ace)
        if aces.Length > 0 then
            match aces |> Array.tryFind (fun c -> c.Suit = Suit.Hearts) with
            | Some ace -> ace
            | None -> aces.[0]
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
                if nonSpades.Length > 0 then
                    leadFromShortestSafe nonSpades mySeat voids
                else
                    legalCards |> Array.minBy (fun c -> c.Rank)
            else
                if lowSpades.Length > 0 then
                    lowSpades |> Array.minBy (fun c -> c.Rank)
                else
                    leadFromShortestSafe legalCards mySeat voids
        else
            let nonQS = legalCards |> Array.filter (fun c -> not (isQS c))
            if nonQS.Length > 0 then
                leadFromShortestSafe nonQS mySeat voids
            else
                leadFromShortestSafe legalCards mySeat voids

    // ============================================================
    // Following Strategy
    // ============================================================

    let private chooseFollowShooting (cardsInSuit : Card[]) (legalCards : Card[]) (highRank : Rank option) =
        if cardsInSuit.Length > 0 then
            match highRank with
            | Some hr ->
                let winningCards = cardsInSuit |> Array.filter (fun c -> c.Rank > hr)
                if winningCards.Length > 0 then
                    winningCards |> Array.minBy (fun c -> c.Rank)
                else
                    cardsInSuit |> Array.minBy (fun c -> c.Rank)
            | None ->
                cardsInSuit |> Array.maxBy (fun c -> c.Rank)
        else
            legalCards |> Array.minBy (fun c -> c.Rank)

    let private tryIntercept (cardsInSuit : Card[]) (highRank : Rank option) =
        match highRank with
        | Some hr ->
            let winningCards = cardsInSuit |> Array.filter (fun c -> c.Rank > hr)
            if winningCards.Length > 0 then
                Some (winningCards |> Array.minBy (fun c -> c.Rank))
            else None
        | None -> None

    let private chooseFollowLastToPlay
        (cardsInSuit : Card[])
        (highRank : Rank option)
        (suitLed : Suit)
        (trickPoints : int)
        (shouldIntercept : bool)
        (unplayedCards : Set<Card>) =

        let queenSpades = cardsInSuit |> Array.tryFind isQS
        match queenSpades, highRank with
        | Some qs, Some hr when suitLed = Suit.Spades && hr > Rank.Queen -> qs
        | _ ->
            if shouldIntercept then
                match tryIntercept cardsInSuit highRank with
                | Some card -> card
                | None ->
                    let nonQS = cardsInSuit |> Array.filter (fun c -> not (isQS c))
                    if nonQS.Length > 0 then nonQS |> Array.maxBy (fun c -> c.Rank)
                    else cardsInSuit |> Array.maxBy (fun c -> c.Rank)
            elif trickPoints <= 0 then
                let nonQS = cardsInSuit |> Array.filter (fun c -> not (isQS c))
                let highestCard =
                    if nonQS.Length > 0 then nonQS |> Array.maxBy (fun c -> c.Rank)
                    else cardsInSuit |> Array.maxBy (fun c -> c.Rank)
                if isHighestRemaining highestCard unplayedCards then highestCard
                elif nonQS.Length > 0 then nonQS |> Array.maxBy (fun c -> c.Rank)
                else cardsInSuit |> Array.maxBy (fun c -> c.Rank)
            else
                match highRank with
                | Some hr ->
                    let belowWinner = cardsInSuit |> Array.filter (fun c -> c.Rank < hr)
                    if belowWinner.Length > 0 then
                        belowWinner |> Array.maxBy (fun c -> c.Rank)
                    else
                        let nonQS = cardsInSuit |> Array.filter (fun c -> not (isQS c))
                        if nonQS.Length > 0 then nonQS |> Array.maxBy (fun c -> c.Rank)
                        else cardsInSuit |> Array.maxBy (fun c -> c.Rank)
                | None ->
                    cardsInSuit |> Array.minBy (fun c -> c.Rank)

    let private chooseFollowNotLast
        (cardsInSuit : Card[])
        (highRank : Rank option)
        (suitLed : Suit)
        (qsPlayed : bool)
        (isSecondToLast : bool)
        (shouldIntercept : bool) =

        // Try to intercept if someone is shooting
        let interceptCard =
            if shouldIntercept then tryIntercept cardsInSuit highRank
            else None

        match interceptCard with
        | Some card -> card
        | None ->
            // Check if we can safely dump QS
            let queenSpades = cardsInSuit |> Array.tryFind isQS
            match queenSpades, highRank with
            | Some qs, Some hr when suitLed = Suit.Spades && hr > Rank.Queen -> qs
            | _ ->
                match highRank with
                | Some hr ->
                    let belowWinner = cardsInSuit |> Array.filter (fun c -> c.Rank < hr)
                    if isSecondToLast && suitLed = Suit.Spades && not qsPlayed then
                        let belowQueen = cardsInSuit |> Array.filter (fun c -> c.Rank < Rank.Queen)
                        if belowQueen.Length > 0 then
                            belowQueen |> Array.maxBy (fun c -> c.Rank)
                        elif belowWinner.Length > 0 then
                            belowWinner |> Array.maxBy (fun c -> c.Rank)
                        else
                            let nonQS = cardsInSuit |> Array.filter (fun c -> not (isQS c))
                            if nonQS.Length > 0 then nonQS |> Array.minBy (fun c -> c.Rank)
                            else cardsInSuit |> Array.minBy (fun c -> c.Rank)
                    elif belowWinner.Length > 0 then
                        belowWinner |> Array.maxBy (fun c -> c.Rank)
                    else
                        let nonQS = cardsInSuit |> Array.filter (fun c -> not (isQS c))
                        if nonQS.Length > 0 then nonQS |> Array.minBy (fun c -> c.Rank)
                        else cardsInSuit |> Array.minBy (fun c -> c.Rank)
                | None ->
                    cardsInSuit |> Array.minBy (fun c -> c.Rank)

    let private chooseDiscard (legalCards : Card[]) (qsPlayed : bool) (holdingQS : bool) =
        match legalCards |> Array.tryFind isQS with
        | Some qs -> qs
        | None ->
            let dumpHighestHeart () =
                let hearts = legalCards |> Array.filter (fun c -> c.Suit = Suit.Hearts)
                if hearts.Length > 0 then
                    Some (hearts |> Array.maxBy (fun c -> c.Rank))
                else None

            let dumpHighestNonHeart () =
                let nonHearts =
                    if holdingQS then
                        legalCards |> Array.filter (fun c -> c.Suit <> Suit.Hearts && c.Suit <> Suit.Spades)
                    else
                        legalCards |> Array.filter (fun c -> c.Suit <> Suit.Hearts)
                if nonHearts.Length > 0 then
                    nonHearts |> Array.maxBy (fun c -> c.Rank)
                else
                    legalCards |> Array.maxBy (fun c -> c.Rank)

            if not qsPlayed && not holdingQS then
                let highSpades = legalCards |> Array.filter (fun c -> c.Suit = Suit.Spades && c.Rank > Rank.Queen)
                if highSpades.Length > 0 then
                    highSpades |> Array.maxBy (fun c -> c.Rank)
                else
                    match dumpHighestHeart () with
                    | Some card -> card
                    | None -> dumpHighestNonHeart ()
            else
                match dumpHighestHeart () with
                | Some card -> card
                | None -> dumpHighestNonHeart ()

    let private chooseFollow (hand : Hand) (deal : ClosedDeal) (trick : Trick) (legalCards : Card[]) (mySeat : Seat) =
        let suitLed = trick.SuitLedOpt |> Option.get
        let trickCount = trick.Cards.Length
        let isLastToPlay = trickCount = 3
        let isSecondToLast = trickCount = 2
        let isFirstTrick = deal.CompletedTricks.IsEmpty

        let qsPlayed = not (deal.UnplayedCards.Contains(queenOfSpades))
        let holdingQS = hand.Contains(queenOfSpades)

        let trickPoints = Trick.pointValue trick
        let highRank = trick.HighPlayOpt |> Option.map (fun (_, c) -> c.Rank)

        let cardsInSuit = legalCards |> Array.filter (fun c -> c.Suit = suitLed)

        // Shooting logic
        if isAttemptingToShoot deal mySeat then
            chooseFollowShooting cardsInSuit legalCards highRank

        // Anti-shooting logic
        else
            let scores = Score.indexed deal.Score
            let (potentialShooter, shooterPoints) = scores |> Seq.maxBy snd
            let isShootingRisk = shooterPoints >= 21 && potentialShooter <> mySeat
            let shooterIsWinning =
                match trick.HighPlayOpt with
                | Some (seat, _) -> seat = potentialShooter
                | None -> false
            let shouldIntercept = isShootingRisk && shooterIsWinning

            if cardsInSuit.Length > 0 then
                if isFirstTrick then
                    cardsInSuit |> Array.maxBy (fun c -> c.Rank)
                elif isLastToPlay then
                    chooseFollowLastToPlay cardsInSuit highRank suitLed trickPoints shouldIntercept deal.UnplayedCards
                else
                    chooseFollowNotLast cardsInSuit highRank suitLed qsPlayed isSecondToLast shouldIntercept
            else
                chooseDiscard legalCards qsPlayed holdingQS

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
                    let deal = infoSet.Deal
                    let hand = infoSet.Hand
                    let seat = infoSet.Player
                    match deal.CurrentTrickOpt with
                    | None -> failwith "No current trick"
                    | Some trick ->
                        if trick.Cards.IsEmpty then
                            chooseLead hand deal legalActions seat
                        else
                            chooseFollow hand deal trick legalActions seat

        { Act = act }
