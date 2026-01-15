namespace Hearts.Heuristic

open System

open PlayingCards
open Hearts
open Hearts.Learn

module Program =

    let rng = Random()

    let eval player =
        Tournament.run rng true 20000 Trickster.player player
            |> snd

    /// Queen of Spades - worth 13 points
    let queenOfSpades = Card.create Rank.Queen Suit.Spades

    /// Check if a card is the Queen of Spades
    let isQS (card : Card) = card.Suit = Suit.Spades && card.Rank = Rank.Queen

    /// Get cards of a specific suit from hand
    let cardsInSuit suit (hand : Hand) =
        hand |> Seq.filter (fun c -> c.Suit = suit) |> Seq.toList

    /// Count cards in suit
    let countInSuit suit (hand : Hand) =
        hand |> Seq.filter (fun c -> c.Suit = suit) |> Seq.length

    /// Passing strategy based on Trickster
    let choosePass (hand : Hand) (legalCards : Card[]) =
        // Count spades
        let highSpades = hand |> Seq.filter (fun c -> c.Suit = Suit.Spades && c.Rank >= Rank.Queen) |> Seq.toList
        let lowSpades = hand |> Seq.filter (fun c -> c.Suit = Suit.Spades && c.Rank < Rank.Queen) |> Seq.length

        // Priority 1: Pass Q♠, K♠, A♠ if we don't have 3+ low spades for protection
        if highSpades.Length > 0 && lowSpades < 3 then
            highSpades
                |> List.sortByDescending (fun c -> c.Rank)
                |> List.filter (fun c -> legalCards |> Array.contains c)
                |> List.tryHead
        else None
        |> Option.orElseWith (fun () ->
            // Priority 2: Pass high hearts (but this is simplified - Trickster passes low hearts first)
            let hearts = legalCards |> Array.filter (fun c -> c.Suit = Suit.Hearts)
            if hearts.Length > 0 then
                hearts |> Array.maxBy (fun c -> c.Rank) |> Some
            else None)
        |> Option.orElseWith (fun () ->
            // Priority 3: Pass high clubs/diamonds
            let clubsDiamonds = legalCards |> Array.filter (fun c -> c.Suit = Suit.Clubs || c.Suit = Suit.Diamonds)
            if clubsDiamonds.Length > 0 then
                clubsDiamonds |> Array.maxBy (fun c -> c.Rank) |> Some
            else None)
        |> Option.defaultWith (fun () ->
            // Fallback: pass highest card
            legalCards |> Array.maxBy (fun c -> c.Rank))

    /// Leading strategy based on Trickster
    let chooseLead (hand : Hand) (deal : ClosedDeal) (legalCards : Card[]) =
        let qsPlayed = not (deal.UnplayedCards.Contains(queenOfSpades))
        let holdingQS = hand.Contains(queenOfSpades)
        let hasHighSpades = hand |> Seq.exists (fun c -> c.Suit = Suit.Spades && c.Rank >= Rank.Queen)

        // Lead low spade if QS still out and we don't hold Q/K/A of spades
        let lowSpades = legalCards |> Array.filter (fun c -> c.Suit = Suit.Spades && c.Rank < Rank.Queen)
        if lowSpades.Length > 0 && not qsPlayed && not hasHighSpades then
            lowSpades |> Array.minBy (fun c -> c.Rank)
        else
            // Otherwise lead lowest non-QS card
            let nonQS = legalCards |> Array.filter (fun c -> not (isQS c))
            if nonQS.Length > 0 then
                // Prefer non-hearts, non-spades
                let safe = nonQS |> Array.filter (fun c -> c.Suit <> Suit.Hearts)
                if safe.Length > 0 then
                    safe |> Array.minBy (fun c -> c.Rank)
                else
                    nonQS |> Array.minBy (fun c -> c.Rank)
            else
                legalCards |> Array.minBy (fun c -> c.Rank)

    /// Following suit strategy based on Trickster
    let chooseFollow (hand : Hand) (deal : ClosedDeal) (trick : Trick) (legalCards : Card[]) =
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
                        // If taking won't hurt (0 points), ditch high card avoiding QS
                        if trickPoints <= 0 then
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

    printfn $"{eval player}"
