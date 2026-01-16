namespace Hearts.Heuristic

open PlayingCards
open Hearts

module Claude =

    /// Queen of Spades - worth 13 points
    let private queenOfSpades = Card.create Rank.Queen Suit.Spades

    /// Check if a card is the Queen of Spades
    let private isQS (card : Card) = card.Suit = Suit.Spades && card.Rank = Rank.Queen

    /// Passing strategy - improved version
    let private choosePass (hand : Hand) (legalCards : Card[]) =
        // Analyze hand by suit
        let spades = hand |> Seq.filter (fun c -> c.Suit = Suit.Spades) |> Seq.toList
        let hearts = hand |> Seq.filter (fun c -> c.Suit = Suit.Hearts) |> Seq.toList
        let clubs = hand |> Seq.filter (fun c -> c.Suit = Suit.Clubs) |> Seq.toList
        let diamonds = hand |> Seq.filter (fun c -> c.Suit = Suit.Diamonds) |> Seq.toList

        let hasQS = spades |> List.exists isQS
        let lowSpades = spades |> List.filter (fun c -> c.Rank < Rank.Queen) |> List.length
        let highSpades = spades |> List.filter (fun c -> c.Rank > Rank.Queen)
        let highHearts = hearts |> List.filter (fun c -> c.Rank >= Rank.Jack) |> List.sortByDescending (fun c -> c.Rank)

        // Identify short suits to void (clubs/diamonds with 1-3 cards)
        // Prioritize shorter suits first (singletons before doubletons)
        let voidCandidates =
            [clubs; diamonds]
            |> List.filter (fun suitCards -> suitCards.Length > 0 && suitCards.Length <= 3)
            |> List.sortBy (fun suitCards -> suitCards.Length)
            |> List.collect id
            |> List.sortByDescending (fun c -> c.Rank)

        // Build priority list of cards to pass
        let candidates =
            [
                // Priority 1: QS if we don't have enough low spades for protection
                if hasQS && lowSpades < 3 then [queenOfSpades] else []

                // Priority 2: High spades (A♠, K♠) - always pass them to avoid winning spade tricks
                if highSpades.Length > 0 then highSpades |> List.sortByDescending (fun c -> c.Rank) else []

                // Priority 3: High hearts (danger cards)
                highHearts

                // Priority 4: Cards from short suits to create voids
                voidCandidates

                // Priority 5: Any high cards (Jack or above)
                legalCards |> Array.filter (fun c -> c.Rank >= Rank.Jack) |> Array.sortByDescending (fun c -> c.Rank) |> Array.toList
            ]
            |> List.concat
            |> List.distinct

        // Pick top candidate that is legal to pass
        candidates
        |> List.tryFind (fun c -> legalCards |> Array.contains c)
        |> Option.defaultValue (legalCards |> Array.maxBy (fun c -> c.Rank))

    /// Lead lowest card from shortest suit (helps create voids)
    /// Avoids leading singleton Aces/Kings which are likely to win tricks
    let private leadFromShortest (cards: Card[]) =
        if cards.Length = 0 then failwith "No cards to lead"
        let grouped =
            cards
            |> Array.groupBy (fun c -> c.Suit)
            |> Array.sortBy (fun (_, suitCards) -> suitCards.Length)

        // Try to find a suit that isn't just a singleton Ace or King
        let safeLead =
            grouped
            |> Array.tryFind (fun (_, suitCards) ->
                let lowest = suitCards |> Array.minBy (fun c -> c.Rank)
                not (suitCards.Length = 1 && lowest.Rank >= Rank.King))

        match safeLead with
        | Some (_, suitCards) -> suitCards |> Array.minBy (fun c -> c.Rank)
        | None -> grouped |> Array.head |> snd |> Array.minBy (fun c -> c.Rank)

    /// Leading strategy - improved version
    let private chooseLead (hand : Hand) (deal : ClosedDeal) (legalCards : Card[]) =
        let qsPlayed = not (deal.UnplayedCards.Contains(queenOfSpades))
        let holdingQS = hand.Contains(queenOfSpades)
        let hasHighSpades = hand |> Seq.exists (fun c -> c.Suit = Suit.Spades && c.Rank > Rank.Queen)

        // Analyze suits in hand
        let lowSpades = legalCards |> Array.filter (fun c -> c.Suit = Suit.Spades && c.Rank < Rank.Queen)

        if not qsPlayed then
            if holdingQS then
                // We have QS - avoid leading spades, lead from shortest non-spade suit
                let nonSpades = legalCards |> Array.filter (fun c -> c.Suit <> Suit.Spades)
                if nonSpades.Length > 0 then
                    leadFromShortest nonSpades
                else
                    // Forced to lead spades - lead lowest
                    legalCards |> Array.minBy (fun c -> c.Rank)
            elif hasHighSpades then
                // We have A/K of spades but not QS - be careful, lead non-spades
                let nonSpades = legalCards |> Array.filter (fun c -> c.Suit <> Suit.Spades)
                if nonSpades.Length > 0 then
                    leadFromShortest nonSpades
                else
                    legalCards |> Array.minBy (fun c -> c.Rank)
            else
                // No dangerous spades - lead low spade to flush out QS
                if lowSpades.Length > 0 then
                    lowSpades |> Array.minBy (fun c -> c.Rank)
                else
                    leadFromShortest legalCards
        else
            // QS is played - safe to lead anything, lead from shortest suit
            let nonQS = legalCards |> Array.filter (fun c -> not (isQS c))
            if nonQS.Length > 0 then
                leadFromShortest nonQS
            else
                leadFromShortest legalCards

    /// Following suit strategy
    let private chooseFollow (hand : Hand) (deal : ClosedDeal) (trick : Trick) (legalCards : Card[]) =
        let suitLed = trick.SuitLedOpt |> Option.get
        let nPlayers = 4
        let trickCount = trick.Cards.Length
        let isLastToPlay = trickCount = nPlayers - 1
        let isSecondToLast = trickCount = nPlayers - 2
        let isFirstTrick = deal.CompletedTricks.IsEmpty

        let qsPlayed = not (deal.UnplayedCards.Contains(queenOfSpades))
        let holdingQS = hand.Contains(queenOfSpades)

        let trickPoints = Trick.pointValue trick
        let cardTakingTrick = trick.HighPlayOpt |> Option.map snd
        let highRank = cardTakingTrick |> Option.map (fun c -> c.Rank)

        // Anti-shooting logic: detect if someone might be shooting the moon
        let scores = Score.indexed deal.Score
        let (potentialShooter, shooterPoints) = scores |> Seq.maxBy snd
        let isShootingRisk = shooterPoints >= 22

        let shooterIsWinning =
            match trick.HighPlayOpt with
            | Some (seat, _) -> seat = potentialShooter
            | None -> false

        let shouldIntercept = isShootingRisk && shooterIsWinning

        // Check if we can follow suit
        let cardsInLedSuit = legalCards |> Array.filter (fun c -> c.Suit = suitLed)
        let canFollowSuit = cardsInLedSuit.Length > 0

        if canFollowSuit then
            // First trick of game - play highest (no points allowed)
            if isFirstTrick then
                cardsInLedSuit |> Array.maxBy (fun c -> c.Rank)
            // Last to play
            elif isLastToPlay then
                // Play QS if it won't win and spades led with higher card taking
                let queenSpades = cardsInLedSuit |> Array.tryFind isQS
                match queenSpades, highRank with
                    | Some qs, Some hr when suitLed = Suit.Spades && hr > Rank.Queen ->
                        qs
                    | _ ->
                        // Anti-shooting: try to win the trick to stop the shooter
                        if shouldIntercept then
                            match highRank with
                            | Some hr ->
                                let winningCards = cardsInLedSuit |> Array.filter (fun c -> c.Rank > hr)
                                if winningCards.Length > 0 then
                                    winningCards |> Array.minBy (fun c -> c.Rank)
                                else
                                    let nonQS = cardsInLedSuit |> Array.filter (fun c -> not (isQS c))
                                    if nonQS.Length > 0 then nonQS |> Array.maxBy (fun c -> c.Rank)
                                    else cardsInLedSuit |> Array.maxBy (fun c -> c.Rank)
                            | None ->
                                cardsInLedSuit |> Array.minBy (fun c -> c.Rank)
                        // If taking won't hurt (0 points), ditch high card avoiding QS
                        elif trickPoints <= 0 then
                            let nonQS = cardsInLedSuit |> Array.filter (fun c -> not (isQS c))
                            if nonQS.Length > 0 then
                                nonQS |> Array.maxBy (fun c -> c.Rank)
                            else
                                cardsInLedSuit |> Array.maxBy (fun c -> c.Rank)
                        else
                            // Try to duck - play highest below winner
                            match highRank with
                                | Some hr ->
                                    let belowWinner = cardsInLedSuit |> Array.filter (fun c -> c.Rank < hr)
                                    if belowWinner.Length > 0 then
                                        belowWinner |> Array.maxBy (fun c -> c.Rank)
                                    else
                                        // Can't duck - play highest avoiding QS
                                        let nonQS = cardsInLedSuit |> Array.filter (fun c -> not (isQS c))
                                        if nonQS.Length > 0 then
                                            nonQS |> Array.maxBy (fun c -> c.Rank)
                                        else
                                            cardsInLedSuit |> Array.maxBy (fun c -> c.Rank)
                                | None ->
                                    cardsInLedSuit |> Array.minBy (fun c -> c.Rank)
            else
                // Not last - try to duck
                // Anti-shooting: try to intercept if we can win
                let attemptIntercept =
                    if shouldIntercept then
                        match highRank with
                        | Some hr ->
                            let winningCards = cardsInLedSuit |> Array.filter (fun c -> c.Rank > hr)
                            if winningCards.Length > 0 then
                                Some (winningCards |> Array.minBy (fun c -> c.Rank))
                            else None
                        | None -> None
                    else None

                match attemptIntercept with
                | Some c -> c
                | None ->
                    // First check if we can safely dump QS
                    let queenSpades = cardsInLedSuit |> Array.tryFind isQS
                    match queenSpades, highRank with
                        | Some qs, Some hr when suitLed = Suit.Spades && hr > Rank.Queen ->
                            // Safe to dump QS - current winner is higher
                            qs
                        | _ ->
                            match highRank with
                                | Some hr ->
                                    let belowWinner = cardsInLedSuit |> Array.filter (fun c -> c.Rank < hr)

                                    // Special case: second to last, spades led, QS not played - stay below Queen
                                    if isSecondToLast && suitLed = Suit.Spades && not qsPlayed then
                                        let belowQueen = cardsInLedSuit |> Array.filter (fun c -> c.Rank < Rank.Queen)
                                        if belowQueen.Length > 0 then
                                            belowQueen |> Array.maxBy (fun c -> c.Rank)
                                        elif belowWinner.Length > 0 then
                                            belowWinner |> Array.maxBy (fun c -> c.Rank)
                                        else
                                            let nonQS = cardsInLedSuit |> Array.filter (fun c -> not (isQS c))
                                            if nonQS.Length > 0 then
                                                nonQS |> Array.minBy (fun c -> c.Rank)
                                            else
                                                cardsInLedSuit |> Array.minBy (fun c -> c.Rank)
                                    elif belowWinner.Length > 0 then
                                        belowWinner |> Array.maxBy (fun c -> c.Rank)
                                    else
                                        // Can't duck - play lowest avoiding QS
                                        let nonQS = cardsInLedSuit |> Array.filter (fun c -> not (isQS c))
                                        if nonQS.Length > 0 then
                                            nonQS |> Array.minBy (fun c -> c.Rank)
                                        else
                                            cardsInLedSuit |> Array.minBy (fun c -> c.Rank)
                                | None ->
                                    cardsInLedSuit |> Array.minBy (fun c -> c.Rank)
        else
            // Can't follow suit - discard
            // Priority 1: Dump QS
            let queenSpades = legalCards |> Array.tryFind isQS
            match queenSpades with
                | Some qs -> qs
                | None ->
                    // Priority 2: Dump A♠/K♠ if QS still out and we don't have it
                    if not qsPlayed && not holdingQS then
                        let highSpades = legalCards |> Array.filter (fun c -> c.Suit = Suit.Spades && c.Rank > Rank.Queen)
                        if highSpades.Length > 0 then
                            highSpades |> Array.maxBy (fun c -> c.Rank)
                        else
                            // Priority 3: Dump highest heart
                            let hearts = legalCards |> Array.filter (fun c -> c.Suit = Suit.Hearts)
                            if hearts.Length > 0 then
                                hearts |> Array.maxBy (fun c -> c.Rank)
                            else
                                // Priority 4: Dump highest non-heart (avoid spades if holding QS)
                                let nonHearts =
                                    if holdingQS then
                                        legalCards |> Array.filter (fun c -> c.Suit <> Suit.Hearts && c.Suit <> Suit.Spades)
                                    else
                                        legalCards |> Array.filter (fun c -> c.Suit <> Suit.Hearts)
                                if nonHearts.Length > 0 then
                                    nonHearts |> Array.maxBy (fun c -> c.Rank)
                                else
                                    legalCards |> Array.maxBy (fun c -> c.Rank)
                    else
                        // QS played or we have it
                        // Dump highest heart
                        let hearts = legalCards |> Array.filter (fun c -> c.Suit = Suit.Hearts)
                        if hearts.Length > 0 then
                            hearts |> Array.maxBy (fun c -> c.Rank)
                        else
                            // Dump highest non-heart (avoid spades if holding QS)
                            let nonHearts =
                                if holdingQS then
                                    legalCards |> Array.filter (fun c -> c.Suit <> Suit.Hearts && c.Suit <> Suit.Spades)
                                else
                                    legalCards |> Array.filter (fun c -> c.Suit <> Suit.Hearts)
                            if nonHearts.Length > 0 then
                                nonHearts |> Array.maxBy (fun c -> c.Rank)
                            else
                                legalCards |> Array.maxBy (fun c -> c.Rank)

    /// Heuristic Hearts player
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
                        match deal.CurrentTrickOpt with
                            | None -> failwith "No current trick"
                            | Some trick ->
                                if trick.Cards.IsEmpty then
                                    chooseLead hand deal legalActions
                                else
                                    chooseFollow hand deal trick legalActions

        { Act = act }
