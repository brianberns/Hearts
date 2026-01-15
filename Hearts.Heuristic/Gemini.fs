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

    /// Passing strategy
    let private choosePass (hand : Hand) (legalCards : Card[]) =
        let spades = hand |> Seq.filter (fun c -> c.Suit = Suit.Spades) |> Seq.toList
        let qs = spades |> List.tryFind isQS
        let lowSpades = spades |> List.filter (fun c -> c.Rank < Rank.Queen) |> List.length
        let highSpades = spades |> List.filter (fun c -> c.Rank > Rank.Queen)

        let hearts = hand |> Seq.filter (fun c -> c.Suit = Suit.Hearts) |> Seq.toList
        let highHearts = hearts |> List.filter (fun c -> c.Rank >= Rank.Jack) |> List.sortByDescending (fun c -> c.Rank)

        let clubs = hand |> Seq.filter (fun c -> c.Suit = Suit.Clubs) |> Seq.toList
        let diamonds = hand |> Seq.filter (fun c -> c.Suit = Suit.Diamonds) |> Seq.toList

        // Target suits to void: Length <= 2
        // We include ALL cards from these suits as candidates
        let voidCandidates = 
            [clubs; diamonds] 
            |> List.filter (fun suitCards -> suitCards.Length <= 2 && suitCards.Length > 0)
            |> List.collect id
            |> List.sortByDescending (fun c -> c.Rank) // Pass high cards of void suit first

        // Selection Logic
        let candidates =
            [
                // Priority 1: Dangerous Spades (QS, AS, KS) if short on spades
                if qs.IsSome && lowSpades < 3 then [qs.Value] else []
                if highSpades.Length > 0 && lowSpades < 3 then highSpades else []
                
                // Priority 2: Void Candidates (Pass ALL cards from short suits)
                voidCandidates

                // Priority 3: High Hearts
                highHearts

                // Priority 4: Any high card
                legalCards 
                |> Array.filter (fun c -> c.Rank >= Rank.Jack) 
                |> Array.sortByDescending (fun c -> c.Rank) 
                |> Array.toList
            ]
            |> List.concat
            |> List.distinct

        // Pick top card from candidates that is in legalCards
        candidates
        |> List.tryFind (fun c -> legalCards |> Array.contains c)
        |> Option.defaultValue (legalCards |> Array.maxBy (fun c -> c.Rank))


    /// Leading strategy
    let private chooseLead (hand : Hand) (deal : ClosedDeal) (legalCards : Card[]) =
        let qsPlayed = not (deal.UnplayedCards.Contains(queenOfSpades))
        let holdingQS = hand.Contains(queenOfSpades)
        
        // If we hold QS, we want to lead a short suit (not Spades) to void it? 
        // Or lead a low Spade to bleed others? No, leading Spade while holding QS is dangerous if others have higher Spades (A, K) to squash it?
        // Actually, leading Spades is illegal if hearts not broken usually? No, Spades is fine.
        
        // If QS is OUT (not played):
        if not qsPlayed then
            // If we have QS:
            if holdingQS then
                // Do not lead Spades.
                // Lead safe low cards in other suits.
                let nonSpades = legalCards |> Array.filter (fun c -> c.Suit <> Suit.Spades)
                if nonSpades.Length > 0 then
                    nonSpades |> Array.minBy (fun c -> c.Rank)
                else
                    // Forced to lead Spades (holding only Spades?)
                    legalCards |> Array.maxBy (fun c -> c.Rank) // Lead high spade? or low? Low.
            else
                // We don't have QS. 
                // Lead Spades if we have low ones (<Q) to flush it out.
                let lowSpades = legalCards |> Array.filter (fun c -> c.Suit = Suit.Spades && c.Rank < Rank.Queen)
                if lowSpades.Length > 0 then
                    lowSpades |> Array.minBy (fun c -> c.Rank)
                else
                    // Lead from a short suit to void it? Or long suit to be safe?
                    // Safe lead: Low card in a suit where we have length?
                    legalCards |> Array.minBy (fun c -> c.Rank)
        else
            // QS is played.
            // Avoid leading point cards (Hearts) unless trying to shoot?
            // If we just want to avoid tricks, lead lowest card.
            // If we are safe (last tricks), maybe lead high to drain others?
            legalCards |> Array.minBy (fun c -> c.Rank)

    /// Following suit strategy
    let private chooseFollow (hand : Hand) (deal : ClosedDeal) (trick : Trick) (legalCards : Card[]) =
        let suitLed = trick.SuitLedOpt |> Option.get
        let nPlayers = 4
        let trickCount = trick.Cards.Length
        let isLastToPlay = trickCount = nPlayers - 1
        
        // Analyze the current trick
        let currentHighPlay = trick.HighPlayOpt
        let currentHighRank = currentHighPlay |> Option.map (fun (_, c) -> c.Rank)
        let currentPoints = Trick.pointValue trick
        
        let cardsInSuit = legalCards |> Array.filter (fun c -> c.Suit = suitLed)
        
        if cardsInSuit.Length > 0 then
            // We MUST follow suit
            
            // 1. Ducking Logic:
            // Can we play under the current high card?
            match currentHighRank with
            | Some highRank ->
                let underCards = cardsInSuit |> Array.filter (fun c -> c.Rank < highRank)
                if underCards.Length > 0 then
                    // We can duck. Play the highest card that is still under the winner (to save lower cards for later safety)
                    // BUT: If the trick has points, definitely duck.
                    // If the trick has NO points, and we are last, maybe win it?
                    
                    if isLastToPlay && currentPoints = 0 && not (deal.UnplayedCards.Contains queenOfSpades) then
                        // Safe to win? 
                        // If QS is gone, and no hearts in trick, winning a cheap trick might be okay to get lead control.
                        // But let's play safe for now.
                        underCards |> Array.maxBy (fun c -> c.Rank)
                    else
                        underCards |> Array.maxBy (fun c -> c.Rank)
                else
                    // We MUST play over.
                    // If we have to win, play the highest card to get rid of it?
                    // Unless it's a point card (Heart/QS) and we don't want to eat our own points (rare, only if suit led is Hearts/Spades).
                    
                    // Specific check for Queen of Spades interaction:
                    // If Spades led, and we have QS, and we must play over... we eat it.
                    // Just play the highest card to dump it.
                    let nonPointHighs = cardsInSuit |> Array.filter (fun c -> not (isPointCard c))
                    if nonPointHighs.Length > 0 then
                        nonPointHighs |> Array.maxBy (fun c -> c.Rank)
                    else
                        cardsInSuit |> Array.minBy (fun c -> c.Rank) // Keep points low if we must eat them? Actually doesn't matter for rank if we win.
            | None ->
                // We are leading? No, follow logic. Should always have high rank if trick not empty.
                // Assuming logic is correct.
                cardsInSuit |> Array.minBy (fun c -> c.Rank)
        else
            // We can DISCARD (Slough)
            // 1. Dump Queen of Spades!
            let qs = legalCards |> Array.tryFind isQS
            if qs.IsSome then qs.Value
            else
                // 2. Dump dangerous Spades (A, K)
                let dangerousSpades = legalCards |> Array.filter (fun c -> c.Suit = Suit.Spades && c.Rank > Rank.Queen)
                if dangerousSpades.Length > 0 then dangerousSpades |> Array.maxBy (fun c -> c.Rank)
                else
                    // 3. Dump high Hearts
                    let hearts = legalCards |> Array.filter (fun c -> c.Suit = Suit.Hearts)
                    if hearts.Length > 0 then hearts |> Array.maxBy (fun c -> c.Rank)
                    else
                        // 4. Dump high cards to void
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