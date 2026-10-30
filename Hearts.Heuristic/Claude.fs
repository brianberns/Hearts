namespace Hearts.Heuristic

open PlayingCards
open Hearts

/// A rule-based Hearts player. Its main ideas:
///
/// - The moon cuts both ways. A moon needs one player to take every point, so once two players
///   have points, nobody can shoot. While no one else has points, this player takes cheap
///   points when its hand could still shoot: that keeps its own moon alive and kills everyone
///   else's. It commits to shooting when it holds all the points and few likely losers.
/// - Riding the edge. With a hand that could still shoot, it sheds middle cards, keeping high
///   cards for control and low ones as exits in case it has to abandon the moon.
/// - Moon defense. Once a single opponent holds all the points taken, this player takes cheap
///   points itself, keeps its points away from that opponent, and dumps points on anyone else.
/// - Card counting. It tracks which cards are still out, so it knows when a card is sure to win.
/// - Ordinary avoidance: pass high cards, duck when possible, shed high cards when safe, and get
///   rid of the Q♠.
module Claude =

    // ============================================================
    // Tuning
    // ============================================================

    /// Points an opponent must hold, all of them, before this player defends against a moon.
    let private threatMinPoints = 3

    /// Most points this player will take to kill a moon threat of any size.
    let private cheapInterceptPoints = 5

    /// Points held by a threat, plus the trick's, above which this player takes any trick to stop it.
    let private seriousThreatPoints = 16

    /// Points a threat must hold before this player leads a sure-winning heart to stop it.
    let private leadHeartThreatPoints = 4

    /// Most likely losers a hand may have for this player to take cheap points while no one
    /// else has any, keeping its own moon alive.
    let private takeMaxLosers = 3

    /// Most points this player takes early to keep its moon alive.
    let private takeMaxPoints = 4

    /// Most likely losers a hand may have for this player to commit to shooting, once it holds
    /// every point taken.
    let private shootMaxLosers = 1

    /// Points this player must hold, all of them, to commit to shooting whatever its hand.
    let private shootingCommitThreshold = 14

    /// Low spades needed to keep the Q♠ when passing.
    let private qsProtectionThreshold = 3

    /// Longest suit to empty when passing.
    let private maxVoidCandidateLength = 3

    // ============================================================
    // Card helpers
    // ============================================================

    let private queenOfSpades = Card.create Rank.Queen Suit.Spades

    let private isQS (card : Card) =
        card.Suit = Suit.Spades && card.Rank = Rank.Queen

    let private bySuit suit (cards : Card[]) =
        cards |> Array.filter (fun c -> c.Suit = suit)

    let private belowRank rank (cards : Card[]) =
        cards |> Array.filter (fun c -> c.Rank < rank)

    let private aboveRank rank (cards : Card[]) =
        cards |> Array.filter (fun c -> c.Rank > rank)

    let private excludingQS (cards : Card[]) =
        cards |> Array.filter (isQS >> not)

    let private nonPoint (cards : Card[]) =
        cards |> Array.filter (fun c -> Card.pointValue c = 0)

    let private highest (cards : Card[]) =
        cards |> Array.maxBy (fun c -> c.Rank)

    let private lowest (cards : Card[]) =
        cards |> Array.minBy (fun c -> c.Rank)

    let private highestPreferNonQS cards =
        let nonQS = excludingQS cards
        if nonQS.Length > 0 then highest nonQS else highest cards

    let private lowestPreferNonQS cards =
        let nonQS = excludingQS cards
        if nonQS.Length > 0 then lowest nonQS else lowest cards

    // ============================================================
    // Situation
    // ============================================================

    /// What this player knows when choosing a play.
    type private Situation =
        {
            Me : Seat
            Hand : Card[]
            Deal : ClosedDeal
            Legal : Card[]

            /// Cards held by the other players.
            Outstanding : Set<Card>

            /// The opponent who has taken every point so far, and how many, if any.
            ThreatOpt : Option<Seat * int>

            /// Has no one but me taken a point?
            MoonOpen : bool

            QSPlayed : bool
        }

    let private createSituation (infoSet : InformationSet) =
        let deal = infoSet.Deal
        let hand = Seq.toArray infoSet.Hand
        let takers =
            Score.indexed deal.Score
                |> Seq.filter (fun (_, points) -> points > 0)
                |> Seq.toArray
        let threatOpt =
            match takers with
                | [| seat, points |] when seat <> infoSet.Player && points >= threatMinPoints ->
                    Some (seat, points)
                | _ -> None
        {
            Me = infoSet.Player
            Hand = hand
            Deal = deal
            Legal = infoSet.LegalActions
            Outstanding = Set.difference deal.UnplayedCards (set hand)
            ThreatOpt = threatOpt
            MoonOpen = takers |> Array.forall (fst >> (=) infoSet.Player)
            QSPlayed = not (deal.UnplayedCards.Contains(queenOfSpades))
        }

    let private isVoid sit seat suit =
        sit.Deal.Voids.Contains(seat, suit)

    /// Players still to play on the current trick after me.
    let private laterPlayers sit (trick : Trick) =
        [ for k in 1 .. Seat.numSeats - 1 - trick.Cards.Length -> Seat.incr k sit.Me ]

    /// Could a player still to play beat the given card in its own suit?
    let private canBeBeaten sit trick (card : Card) =
        laterPlayers sit trick
            |> List.exists (fun seat -> not (isVoid sit seat card.Suit))
            && sit.Outstanding |> Set.exists (fun c -> c.Suit = card.Suit && c.Rank > card.Rank)

    /// Is the given card higher than every card of its suit still out?
    let private isTopCard sit (card : Card) =
        sit.Outstanding |> Set.forall (fun c -> c.Suit <> card.Suit || c.Rank < card.Rank)

    /// Is the given card likely to lose a trick? It is if more higher cards of its suit are out
    /// than I hold myself, unless my higher cards outnumber all of that suit still out.
    let private isLoser sit (card : Card) =
        let theirs = sit.Outstanding |> Seq.filter (fun c -> c.Suit = card.Suit) |> Seq.toArray
        let theirHigher = theirs |> Array.filter (fun c -> c.Rank > card.Rank) |> Array.length
        let myHigher =
            sit.Hand |> Array.filter (fun c -> c.Suit = card.Suit && c.Rank > card.Rank) |> Array.length
        theirHigher > myHigher && myHigher < theirs.Length

    let private countLosers sit =
        sit.Hand |> Array.filter (isLoser sit) |> Array.length

    /// Am I going for the moon? Only while I hold every point taken, and either enough of them
    /// to commit, or a hand with few likely losers.
    let private isShooting sit =
        let mine = sit.Deal.Score[sit.Me]
        sit.MoonOpen
            && mine > 0
            && (mine >= shootingCommitThreshold || countLosers sit <= shootMaxLosers)

    /// Riding the edge: no one else has points, and my hand could still shoot.
    let private isOnEdge sit =
        sit.MoonOpen && countLosers sit <= takeMaxLosers

    // ============================================================
    // Passing
    // ============================================================

    /// Detect if initial hand is strong enough to attempt shooting.
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

    let private choosePassOffensive legalCards =
        let byRank = legalCards |> Array.sortBy (fun (c : Card) -> c.Rank)
        let lowMinorCards =
            byRank |> Array.filter (fun c -> c.Suit <> Suit.Hearts && c.Suit <> Suit.Spades)
        if lowMinorCards.Length > 0 then lowMinorCards[0] else byRank[0]

    let private choosePassDefensive (hand : Hand) legalCards =
        let ofSuit suit = hand |> Seq.filter (fun c -> c.Suit = suit) |> Seq.toList
        let spades = ofSuit Suit.Spades
        let hearts = ofSuit Suit.Hearts

        let hasQS = spades |> List.exists isQS
        let lowSpades = spades |> List.filter (fun c -> c.Rank < Rank.Queen) |> List.length
        let highSpades = spades |> List.filter (fun c -> c.Rank > Rank.Queen)
        let hasHighSpadeProtection =
            highSpades.Length >= 2 || (highSpades.Length >= 1 && lowSpades >= 2)
        let qsUnprotected = lowSpades < qsProtectionThreshold && not hasHighSpadeProtection

        let highHearts =
            hearts
                |> List.filter (fun c -> c.Rank >= Rank.Queen)
                |> List.sortByDescending (fun c -> c.Rank)

            // a short minor suit I can empty
        let voidCandidates =
            [ofSuit Suit.Clubs; ofSuit Suit.Diamonds]
                |> List.filter (fun suit -> suit.Length > 0 && suit.Length <= maxVoidCandidateLength)
                |> List.sortBy List.length
                |> List.collect (List.sortByDescending (fun c -> c.Rank))

        let candidates =
            [
                if hasQS && qsUnprotected then [queenOfSpades] else []
                highSpades |> List.sortByDescending (fun c -> c.Rank)
                highHearts
                voidCandidates
                legalCards
                    |> Array.filter (fun (c : Card) -> c.Rank >= Rank.Jack && not (isQS c))
                    |> Array.sortByDescending (fun c -> c.Rank)
                    |> Array.toList
            ]
                |> List.concat
                |> List.distinct

        candidates
            |> List.tryFind (fun c -> legalCards |> Array.contains c)
            |> Option.defaultValue (highestPreferNonQS legalCards)

    let private choosePass (infoSet : InformationSet) =
        if isShootingHand infoSet.Hand then choosePassOffensive infoSet.LegalActions
        else choosePassDefensive infoSet.Hand infoSet.LegalActions

    // ============================================================
    // Leading
    // ============================================================

    let private leadFromShortestSafe sit (cards : Card[]) =
        let grouped =
            cards
                |> Array.groupBy (fun c -> c.Suit)
                |> Array.sortBy (fun (_, suitCards) -> suitCards.Length)

        let isSingletonHigh (suitCards : Card[]) =
            suitCards.Length = 1 && (lowest suitCards).Rank >= Rank.King

        let anyOpponentVoid suit =
            sit.Deal.Voids |> Set.exists (fun (seat, s) -> seat <> sit.Me && s = suit)

        let isSafeLead (suit, suitCards) =
            not (isSingletonHigh suitCards) && not (anyOpponentVoid suit)

        match grouped |> Array.tryFind isSafeLead with
            | Some (_, suitCards) -> lowest suitCards
            | None ->
                match grouped |> Array.tryFind (snd >> isSingletonHigh >> not) with
                    | Some (_, suitCards) -> lowest suitCards
                    | None -> grouped[0] |> snd |> lowest

    /// Shooting: cash a sure winner, hearts last so they're drawn when no one can escape them;
    /// otherwise lead my highest card.
    let private chooseLeadShooting sit =
        let winners = sit.Legal |> Array.filter (isTopCard sit)
        if winners.Length > 0 then
            let nonHearts = winners |> Array.filter (fun c -> c.Suit <> Suit.Hearts)
            if nonHearts.Length > 0 then highest nonHearts else highest winners
        else highest sit.Legal

    /// Against a moon threat, take a heart trick myself with a heart nobody can beat.
    let private tryLeadAgainstThreat sit =
        match sit.ThreatOpt with
            | Some (_, points) when points >= leadHeartThreatPoints ->
                sit.Legal
                    |> bySuit Suit.Hearts
                    |> Array.filter (isTopCard sit)
                    |> Array.tryHead
            | _ -> None

    let private chooseLead sit =
        if isShooting sit then
            chooseLeadShooting sit
        else
            match tryLeadAgainstThreat sit with
                | Some card -> card
                | None ->
                    let holdingQS = sit.Hand |> Array.exists isQS
                    let hasHighSpades =
                        sit.Hand |> Array.exists (fun c -> c.Suit = Suit.Spades && c.Rank > Rank.Queen)
                    let lowSpades = sit.Legal |> bySuit Suit.Spades |> belowRank Rank.Queen

                    if not sit.QSPlayed then
                        if holdingQS || hasHighSpades then
                            let nonSpades = sit.Legal |> Array.filter (fun c -> c.Suit <> Suit.Spades)
                            if nonSpades.Length > 0 then leadFromShortestSafe sit nonSpades
                            else lowest sit.Legal
                        elif lowSpades.Length > 0 then lowest lowSpades   // flush out the Q♠
                        else leadFromShortestSafe sit sit.Legal
                    else
                        let nonQS = excludingQS sit.Legal
                        leadFromShortestSafe sit (if nonQS.Length > 0 then nonQS else sit.Legal)

    // ============================================================
    // Following
    // ============================================================

    /// Shooting: win with the cheapest card nobody can overtake, else my highest; when I can't
    /// win, keep my high cards.
    let private chooseFollowShooting sit (trick : Trick) (cardsInSuit : Card[]) =
        let _, winningCard = trick.HighPlayOpt.Value
        let winners = cardsInSuit |> aboveRank winningCard.Rank
        let sure = winners |> Array.filter (canBeBeaten sit trick >> not)
        if sure.Length > 0 then lowest sure
        elif winners.Length > 0 then highest winners
        else lowest cardsInSuit

    /// Shooting and void: throw a low card that isn't a point.
    let private chooseDiscardShooting sit =
        let safe = nonPoint sit.Legal
        lowest (if safe.Length > 0 then safe else sit.Legal)

    /// Take a trick worth taking, from a moon threat or to keep my own moon alive.
    let private tryTake sit (trick : Trick) (cardsInSuit : Card[]) =
        let winner, winningCard = trick.HighPlayOpt.Value
        let trickPoints = Trick.pointValue trick
        let winners = cardsInSuit |> aboveRank winningCard.Rank |> excludingQS
        let sure = winners |> Array.filter (canBeBeaten sit trick >> not)
        let interceptThreat =
            match sit.ThreatOpt with
                | Some (threat, threatPoints) when winner = threat ->
                    (trickPoints > 0 && trickPoints <= cheapInterceptPoints)
                        || threatPoints + trickPoints >= seriousThreatPoints
                | _ -> false
        let keepMoonAlive =
            trickPoints > 0 && trickPoints <= takeMaxPoints && isOnEdge sit
        if interceptThreat && winners.Length > 0 then
            Some (if sure.Length > 0 then lowest sure else highest winners)
        elif keepMoonAlive && sure.Length > 0 then
            Some (lowest sure)
        else None

    let private chooseFollowNormal sit (trick : Trick) (cardsInSuit : Card[]) =
        let suitLed = cardsInSuit[0].Suit
        let trickPoints = Trick.pointValue trick
        let _, winningCard = trick.HighPlayOpt.Value
        let isLast = trick.Cards.Length = Seat.numSeats - 1
        let belowWinner = cardsInSuit |> belowRank winningCard.Rank
        let onEdge = isOnEdge sit

        match cardsInSuit |> Array.tryFind isQS with
            | Some qs when winningCard.Rank > Rank.Queen && not onEdge ->
                qs   // dump the Q♠ under a higher spade
            | _ when onEdge && trickPoints = 0 ->
                    // nothing at stake: shed a middle card, keeping my high cards for control and
                    // my low ones as exits in case I have to abandon the moon
                if belowWinner.Length > 0 then highest belowWinner
                else lowestPreferNonQS cardsInSuit
            | _ ->
                if isLast then
                    if trickPoints = 0 then highestPreferNonQS cardsInSuit   // free to shed a high card
                    elif belowWinner.Length > 0 then highest belowWinner
                    else highestPreferNonQS cardsInSuit
                elif belowWinner.Length > 0 then
                        // duck as high as possible, under the Q♠'s rank while it's out
                    if suitLed = Suit.Spades && not sit.QSPlayed then
                        let belowQueen = belowWinner |> belowRank Rank.Queen
                        if belowQueen.Length > 0 then highest belowQueen
                        else highest belowWinner
                    else highest belowWinner
                else
                        // I'm winning for now. If nobody can overtake me, or nothing is at stake,
                        // shed my highest card.
                    let safeToShed =
                        not (canBeBeaten sit trick (lowest cardsInSuit))
                            || (trickPoints = 0
                                && suitLed <> Suit.Hearts
                                && (suitLed <> Suit.Spades || sit.QSPlayed))
                    if safeToShed then highestPreferNonQS cardsInSuit
                    else lowestPreferNonQS cardsInSuit

    /// The best non-point card to throw away: a dangerous high spade, else the highest card of my
    /// shortest suit.
    let private safeDiscard sit (candidates : Card[]) =
        let holdingQS = sit.Hand |> Array.exists isQS
        let highSpades = candidates |> bySuit Suit.Spades |> aboveRank Rank.Queen
        if not sit.QSPlayed && not holdingQS && highSpades.Length > 0 then
            highest highSpades
        else
            let eligible =
                if holdingQS then candidates |> Array.filter (fun c -> c.Suit <> Suit.Spades)
                else candidates
            let eligible = if eligible.Length > 0 then eligible else candidates
            let suitLength suit = sit.Hand |> Array.filter (fun c -> c.Suit = suit) |> Array.length
            eligible
                |> Array.groupBy (fun c -> c.Suit)
                |> Array.minBy (fst >> suitLength)
                |> snd
                |> highest

    let private chooseDiscard sit (trick : Trick) =
        let feedsThreat =
            match sit.ThreatOpt, trick.HighPlayOpt with
                | Some (threat, _), Some (winner, _) ->
                    winner = threat || List.contains threat (laterPlayers sit trick)
                | _ -> false
        let safe = nonPoint sit.Legal
        let safeLosers = safe |> Array.filter (isLoser sit)
        if isOnEdge sit && safeLosers.Length > 0 then
            highest safeLosers   // keep my moon alive with one less loser, but keep my low exits
        elif feedsThreat && safe.Length > 0 then
            safeDiscard sit safe   // keep points away from a possible shooter
        else
            match sit.Legal |> Array.tryFind isQS with
                | Some qs -> qs
                | None ->
                    let hearts = sit.Legal |> bySuit Suit.Hearts
                    let holdingQS = sit.Hand |> Array.exists isQS
                    let dangerousSpades = sit.Legal |> bySuit Suit.Spades |> aboveRank Rank.Queen
                    if not sit.QSPlayed && not holdingQS && dangerousSpades.Length > 0 then
                        highest dangerousSpades
                    elif hearts.Length > 0 then highest hearts
                    else safeDiscard sit sit.Legal

    let private chooseFollow sit (trick : Trick) =
        let suitLed = trick.SuitLedOpt.Value
        let cardsInSuit = sit.Legal |> bySuit suitLed

        if cardsInSuit.Length = 0 then
            if isShooting sit then chooseDiscardShooting sit
            else chooseDiscard sit trick
        elif sit.Deal.CompletedTricks.IsEmpty then
            highest cardsInSuit   // no points on the first trick
        elif isShooting sit then
            chooseFollowShooting sit trick cardsInSuit
        else
            tryTake sit trick cardsInSuit
                |> Option.defaultWith (fun () -> chooseFollowNormal sit trick cardsInSuit)

    // ============================================================
    // Player
    // ============================================================

    let player =
        let act (infoSet : InformationSet) =
            if infoSet.LegalActions.Length = 1 then
                infoSet.LegalActions[0]
            else
                match infoSet.LegalActionType with
                    | ActionType.Pass ->
                        choosePass infoSet
                    | ActionType.Play ->
                        let sit = createSituation infoSet
                        let trick = ClosedDeal.currentTrick infoSet.Deal
                        if trick.Cards.IsEmpty then chooseLead sit
                        else chooseFollow sit trick

        { Act = act }
