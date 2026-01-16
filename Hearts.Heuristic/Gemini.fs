namespace Hearts.Heuristic

open PlayingCards
open Hearts

module Gemini =

    /// Queen of Spades - worth 13 points
    let private queenOfSpades = Card.create Rank.Queen Suit.Spades

    /// Check if a card is the Queen of Spades
    let private isQS (card : Card) = card.Suit = Suit.Spades && card.Rank = Rank.Queen

    /// Check if a card is a point card
    let private isPointCard (card : Card) =
        card.Suit = Suit.Hearts || isQS card

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
        let voidCandidates = 
            [clubs; diamonds] 
            |> List.filter (fun suitCards -> suitCards.Length > 0 && suitCards.Length <= 3)
            |> List.collect id
            |> List.sortByDescending (fun c -> c.Rank)

        // Build priority list of cards to pass
        let candidates =
            [
                // Priority 1: QS if we don't have enough low spades for protection
                if hasQS && lowSpades < 3 then [queenOfSpades] else []

                // Priority 2: High spades (A, K) - Always pass
                if highSpades.Length > 0 then highSpades |> List.sortByDescending (fun c -> c.Rank) else []

                // Priority 3: High hearts (Danger cards)
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

    /// Helper to detect if we should commit to Shooting the Moon
    let private isAttemptingToShoot (deal : ClosedDeal) (mySeat : Seat) =
        let myPoints = deal.Score[mySeat]
        let totalPointsTaken = deal.Score |> Score.sum
        
        // Conditions:
        // 1. We have taken ALL points so far.
        // 2. We have a significant number of points (>= 18) to justify the risk.
        //    (13 for QS + 5 hearts = 18. If we have QS and 5 hearts, we need to run the rest).
        myPoints > 0 && myPoints = totalPointsTaken && myPoints >= 18

    /// Leading strategy - improved version
    let private chooseLead (hand : Hand) (deal : ClosedDeal) (legalCards : Card[]) (mySeat : Seat) =
        // Check offensive shooting
        if isAttemptingToShoot deal mySeat then
            // If shooting, lead highest cards to maintain control
            legalCards |> Array.maxBy (fun c -> c.Rank)
        else
            // Normal Defensive Strategy
            let qsPlayed = not (deal.UnplayedCards.Contains(queenOfSpades))
            let holdingQS = hand.Contains(queenOfSpades)
            let hasHighSpades = hand |> Seq.exists (fun c -> c.Suit = Suit.Spades && c.Rank > Rank.Queen)

            // Analyze suits in hand
            let spadesInHand = legalCards |> Array.filter (fun c -> c.Suit = Suit.Spades)
            let lowSpades = spadesInHand |> Array.filter (fun c -> c.Rank < Rank.Queen)

            // Helper to pick lowest card from shortest suit
            let leadFromShortest (cards: Card[]) =
                let grouped = 
                    cards
                    |> Array.groupBy (fun c -> c.Suit)
                    |> Array.sortBy (fun (_, suitCards) -> suitCards.Length)
                
                let safeLead = 
                    grouped 
                    |> Array.tryFind (fun (_, suitCards) -> 
                        let lowest = suitCards |> Array.minBy (fun c -> c.Rank)
                        not (suitCards.Length = 1 && lowest.Rank >= Rank.King))
                
                match safeLead with
                | Some (_, suitCards) -> suitCards |> Array.minBy (fun c -> c.Rank)
                | None -> grouped |> Array.head |> snd |> Array.minBy (fun c -> c.Rank)

            if not qsPlayed then
                if holdingQS then
                    let nonSpades = legalCards |> Array.filter (fun c -> c.Suit <> Suit.Spades)
                    if nonSpades.Length > 0 then
                        leadFromShortest nonSpades
                    else
                        legalCards |> Array.minBy (fun c -> c.Rank)
                elif hasHighSpades then
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
                        // No low spades? Lead shortest safe suit
                        leadFromShortest legalCards
            else
                // QS is played - safe to lead anything, prefer low cards from short suits to void
                let nonQS = legalCards |> Array.filter (fun c -> not (isQS c))
                if nonQS.Length > 0 then
                    leadFromShortest nonQS
                else
                    leadFromShortest legalCards

    /// Following suit strategy
    let private chooseFollow (hand : Hand) (deal : ClosedDeal) (trick : Trick) (legalCards : Card[]) (mySeat : Seat) =
        let suitLed = trick.SuitLedOpt |> Option.get
        let nPlayers = 4
        let trickCount = trick.Cards.Length
        let isLastToPlay = trickCount = nPlayers - 1
        let isSecondToLast = trickCount = nPlayers - 2
        
        let trickPoints = Trick.pointValue trick
        let cardTakingTrick = trick.HighPlayOpt |> Option.map snd
        let highRank = cardTakingTrick |> Option.map (fun c -> c.Rank)

        // Check offensive shooting
        if isAttemptingToShoot deal mySeat then
            // If shooting, try to WIN the trick with highest card
            // But if we can't win, dump low
            let cardsInSuit = legalCards |> Array.filter (fun c -> c.Suit = suitLed)
            if cardsInSuit.Length > 0 then
                // Following suit
                match highRank with
                | Some hr ->
                    let winningCards = cardsInSuit |> Array.filter (fun c -> c.Rank > hr)
                    if winningCards.Length > 0 then
                        // Win as cheaply as possible to save higher cards for later tricks
                        winningCards |> Array.minBy (fun c -> c.Rank)
                    else
                        // Can't win? Play lowest (save high for later opportunities? or dump?)
                        // If we lose trick, shooting fails. So it doesn't matter much.
                        cardsInSuit |> Array.minBy (fun c -> c.Rank)
                | None -> 
                    // We are leading? No, follow.
                    cardsInSuit |> Array.maxBy (fun c -> c.Rank)
            else
                // Discarding
                // If shooting, discard low cards to keep high ones for winning?
                // Or discard point cards? No, we want to TAKE points.
                // Discard lowest useless card.
                legalCards |> Array.minBy (fun c -> c.Rank)
        else
            // Normal Defensive Strategy
            let qsPlayed = not (deal.UnplayedCards.Contains(queenOfSpades))
            let holdingQS = hand.Contains(queenOfSpades)

            // Anti-Shooting Logic (Defensive)
            let dealScore = deal.Score
            let scores = Score.indexed dealScore 
            let (potentialShooter, shooterPoints) = scores |> Seq.maxBy snd
            // Increased threshold to 21
            let isShootingRisk = shooterPoints >= 21

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
                if deal.CompletedTricks.IsEmpty then
                    cardsInLedSuit |> Array.maxBy (fun c -> c.Rank)
                // Last to play
                elif isLastToPlay then
                    let queenSpades = cardsInLedSuit |> Array.tryFind isQS
                    match queenSpades, highRank with
                        | Some qs, Some hr when suitLed = Suit.Spades && hr > Rank.Queen ->
                            qs
                        | _ ->
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
                            elif trickPoints <= 0 then
                                let nonQS = cardsInLedSuit |> Array.filter (fun c -> not (isQS c))
                                if nonQS.Length > 0 then
                                    nonQS |> Array.maxBy (fun c -> c.Rank)
                                else
                                    cardsInLedSuit |> Array.maxBy (fun c -> c.Rank)
                            else
                                match highRank with
                                    | Some hr ->
                                        let belowWinner = cardsInLedSuit |> Array.filter (fun c -> c.Rank < hr)
                                        if belowWinner.Length > 0 then
                                            belowWinner |> Array.maxBy (fun c -> c.Rank)
                                        else
                                            let nonQS = cardsInLedSuit |> Array.filter (fun c -> not (isQS c))
                                            if nonQS.Length > 0 then
                                                nonQS |> Array.maxBy (fun c -> c.Rank)
                                            else
                                                cardsInLedSuit |> Array.maxBy (fun c -> c.Rank)
                                    | None ->
                                        cardsInLedSuit |> Array.minBy (fun c -> c.Rank)
                else
                    // Not last - try to duck
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
                        let queenSpades = cardsInLedSuit |> Array.tryFind isQS
                        match queenSpades, highRank with
                            | Some qs, Some hr when suitLed = Suit.Spades && hr > Rank.Queen ->
                                qs
                            | _ ->
                                match highRank with
                                    | Some hr ->
                                        let belowWinner = cardsInLedSuit |> Array.filter (fun c -> c.Rank < hr)
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
                                            let nonQS = cardsInLedSuit |> Array.filter (fun c -> not (isQS c))
                                            if nonQS.Length > 0 then
                                                nonQS |> Array.minBy (fun c -> c.Rank)
                                            else
                                                cardsInLedSuit |> Array.minBy (fun c -> c.Rank)
                                    | None ->
                                        cardsInLedSuit |> Array.minBy (fun c -> c.Rank)
            else
                // Discard
                let queenSpades = legalCards |> Array.tryFind isQS
                match queenSpades with
                    | Some qs -> qs
                    | None ->
                        if not qsPlayed && not holdingQS then
                            let highSpades = legalCards |> Array.filter (fun c -> c.Suit = Suit.Spades && c.Rank > Rank.Queen)
                            if highSpades.Length > 0 then
                                highSpades |> Array.maxBy (fun c -> c.Rank)
                            else
                                let hearts = legalCards |> Array.filter (fun c -> c.Suit = Suit.Hearts)
                                if hearts.Length > 0 then
                                    hearts |> Array.maxBy (fun c -> c.Rank)
                                else
                                    let nonHearts = legalCards |> Array.filter (fun c -> c.Suit <> Suit.Hearts)
                                    if nonHearts.Length > 0 then
                                        nonHearts |> Array.maxBy (fun c -> c.Rank)
                                    else
                                        legalCards |> Array.maxBy (fun c -> c.Rank)
                        else
                            let hearts = legalCards |> Array.filter (fun c -> c.Suit = Suit.Hearts)
                            if hearts.Length > 0 then
                                hearts |> Array.maxBy (fun c -> c.Rank)
                            else
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
                                    chooseLead hand deal legalActions infoSet.Player
                                else
                                    chooseFollow hand deal trick legalActions infoSet.Player

        { Act = act }