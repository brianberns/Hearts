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
/// - Moon defense. Once a single opponent holds enough points to be a probable shooter, this
///   player fights for tricks the shooter leads, keeps its stoppers when it can't win, keeps its
///   points away from the shooter, and dumps them on anyone else.
/// - Card counting. It tracks which cards are still out, so it knows when a card is sure to win.
/// - Passing to lie low: Killer Hearts' ordered pass rules, which keep the high cards that protect
///   a long suit and pass the ones that are liabilities.
/// - Ordinary avoidance: duck when possible, shed high cards when safe, and get rid of the Q♠.
///   Holding the Q♠, it empties a short suit to make a place to dump it.
/// - Lead losers. It doesn't lead a card that's sure to win a trick once points are split.
///
/// Several of these ideas come from Killer Hearts, Gerald Berns' Hearts program.
module Claude =

    // ============================================================
    // Tuning
    // ============================================================

    /// Points that make a lone point-taker a probable shooter. The Q♠ alone, perhaps with one
    /// heart, is usually a dump, not a shoot.
    let private probableShooterPoints = 6

    /// Tricks played, after which any lone point-taker is a probable shooter.
    let private lateTricks = 9

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

    let private isMinor (card : Card) =
        card.Suit = Suit.Clubs || card.Suit = Suit.Diamonds

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

            /// The opponent who has taken every point so far, and how many, if a probable shooter.
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
        let isProbableShooter points =
            (points >= probableShooterPoints && points <> 13 && points <> 14)
                || deal.CompletedTricks.Length >= lateTricks
        let threatOpt =
            match takers with
                | [| seat, points |] when seat <> infoSet.Player && isProbableShooter points ->
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

    /// Is any opponent known to be void in the given suit?
    let private anyOpponentVoid sit suit =
        sit.Deal.Voids |> Set.exists (fun (seat, s) -> seat <> sit.Me && s = suit)

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

    /// Killer Hearts' pass to lie low, as Gerald Berns wrote it for a game score of zero: an
    /// ordered list of rules, each of which may choose the next card to pass, re-evaluated on
    /// the remaining hand after each choice. It passes the Q♠ and the A♠/K♠ only from a short
    /// suit, empties short clubs and diamonds, and passes the high cards of suits whose low
    /// cards are dangerous, but keeps the high hearts and spades that protect a long suit.
    module private LieLow =

        let private rankOf (card : Card) = int card.Rank

        let private jack = int Rank.Jack
        let private queen = int Rank.Queen
        let private king = int Rank.King
        let private ace = int Rank.Ace

        /// Tricks a suit is expected to take, by the number of its cards the opponents hold.
        let private estimatedTricks = [| 0; 1; 1; 2; 3; 3; 3; 4; 4; 5; 6; 6; 6; 6 |]

        /// Number of cards at the top of a suit (highest first) that run down from the ace.
        let private topRun (cards : Card[]) =
            cards
                |> Seq.indexed
                |> Seq.takeWhile (fun (i, c) -> rankOf c = ace - i)
                |> Seq.length

        /// Low cards of a suit (highest first) likely to lose a trick before it wins one, judged from the
        /// bottom up as opponents' cards fall.
        let private defensiveLosers (cards : Card[]) =
            let length = cards.Length
            if length = 0 then 0
            else
                let run = topRun cards
                let tricks = min estimatedTricks[13 - length] length
                let rec loop j opponentsHave opponentsPlayed voidOpponents winners losers =
                    if j < length - tricks then
                        if winners = 0 then length else losers
                    else
                        let wins =
                            opponentsHave < 3 * ((rankOf cards[j] - 1 - length + j) - (opponentsPlayed + 1) / 2)
                                || j < run
                        let losers = if wins && winners = 0 then length - 1 - j else losers
                        let winners = if wins then winners + 1 else winners
                        let opponentsPlayed = min (13 - length) (opponentsPlayed + 3 - voidOpponents)
                        let opponentsHave = max 0 (opponentsHave - (3 - voidOpponents))
                        let voidOpponents =
                            if voidOpponents = 0 && opponentsPlayed >= 3 then 1
                            elif voidOpponents = 1 && opponentsPlayed >= 6 then 2
                            else voidOpponents
                        loop (j - 1) opponentsHave opponentsPlayed voidOpponents winners losers
                loop (length - 1) (13 - length) 0 0 0 0

        /// What the rules need to know about the hand that's left.
        type private Shape =
            {
                /// Each suit's cards, highest first.
                Cards : Map<Suit, Card[]>
                DefensiveLosers : Map<Suit, int>
                TopRun : Map<Suit, int>

                /// Clubs or diamonds of each length; on a tie, the weaker of the two.
                MinorByLength : Map<int, Suit>
                Goren : int

                /// Clubs, diamonds or hearts, whichever has the highest low cards, and how high they are.
                WorstSuit : Option<Suit * int>
            }

        let private suits = [ Suit.Spades; Suit.Hearts; Suit.Clubs; Suit.Diamonds ]

        let private createShape (hand : Card[]) =
            let cards =
                suits
                    |> List.map (fun suit ->
                        suit, hand |> Array.filter (fun c -> c.Suit = suit) |> Array.sortByDescending rankOf)
                    |> Map
            let losers = cards |> Map.map (fun _ cs -> defensiveLosers cs)
            let clubs, diamonds = cards[Suit.Clubs], cards[Suit.Diamonds]
            let minorByLength =
                let byLength = Map [ clubs.Length, Suit.Clubs ]
                if diamonds.Length = 0 then byLength
                elif diamonds.Length <> clubs.Length then byLength.Add(diamonds.Length, Suit.Diamonds)
                elif clubs.Length = 1 then byLength   // the 2♣ leads: prefer clubs
                elif losers[Suit.Diamonds] < losers[Suit.Clubs] then byLength.Add(diamonds.Length, Suit.Diamonds)
                elif losers[Suit.Diamonds] > losers[Suit.Clubs] then byLength
                else
                        // compare from the bottom up
                    let pairs = Array.zip clubs diamonds |> Array.rev
                    match pairs |> Array.tryFind (fun (c, d) -> rankOf c <> rankOf d) with
                        | Some (c, d) when rankOf c < rankOf d -> byLength.Add(diamonds.Length, Suit.Diamonds)
                        | _ -> byLength
            let goren =
                suits |> List.sumBy (fun suit ->
                    let cs = cards[suit]
                    let shape =
                        if cs.Length <= 2 then 3 - cs.Length
                        elif cs.Length >= 5 && rankOf cs[0] >= jack then min cs.Length 7 - 4
                        else 0
                    let honors = cs |> Seq.takeWhile (fun c -> rankOf c >= jack) |> Seq.sumBy (fun c -> rankOf c - 10)
                    let queenOfSpades =
                        if suit = Suit.Spades && cs.Length >= 5 && cs |> Array.exists (fun c -> rankOf c = queen) then 1
                        else 0
                    shape + honors + queenOfSpades)
            let worstSuit =
                [ Suit.Hearts; Suit.Clubs; Suit.Diamonds ]
                    |> List.map (fun suit ->
                        let low = cards[suit] |> Array.rev |> Array.truncate 3
                        suit, low |> Array.mapi (fun t c -> rankOf c - 2 - t) |> Array.sum)
                    |> List.fold (fun best (suit, score) ->
                        match best with
                            | Some (_, top) when score <= top -> best
                            | _ -> Some (suit, score)) None
            {
                Cards = cards
                DefensiveLosers = losers
                TopRun = cards |> Map.map (fun _ cs -> topRun cs)
                MinorByLength = minorByLength
                Goren = goren
                WorstSuit = worstSuit
            }

        /// What's been decided so far in this pass.
        type private Progress =
            {
                Step : int
                Direction : ExchangeDirection
                PassedQS : bool
                PassedHeart : bool
            }

        /// Chooses the next card to pass, by the first rule that applies.
        let private chooseNext (shape : Shape) (progress : Progress) =
            let cards suit = shape.Cards[suit]
            let length suit = (cards suit).Length
            let high suit = if length suit = 0 then 0 else rankOf ((cards suit)[0])
            let low suit = if length suit = 0 then 0 else rankOf (Array.last (cards suit))
            let card suit position = Some ((cards suit)[position])
            let losers suit = shape.DefensiveLosers[suit]
            let run suit = shape.TopRun[suit]
            let minorOfLength n = shape.MinorByLength.TryFind n

            let spades = cards Suit.Spades
            let nSpades = spades.Length
            let qsPosition = spades |> Array.tryFindIndex (fun c -> rankOf c = queen)
            let holdingQS = qsPosition.IsSome
            let highSpades =
                if nSpades >= 2 && rankOf spades[1] = king then 2
                elif nSpades >= 1 && rankOf spades[0] >= king then 1
                else 0
            let passedQSRight = progress.PassedQS && progress.Direction = ExchangeDirection.Right
            let passedQSLeft = progress.PassedQS && progress.Direction = ExchangeDirection.Left
            let smallClubs = length Suit.Clubs > 0 && length Suit.Clubs <= 4 - progress.Step
            let nHearts = length Suit.Hearts
            let highHeart = high Suit.Hearts
            let beatableHeart = nHearts >= 16 - highHeart
            let dangerOfDeal = shape.WorstSuit |> Option.map snd |> Option.defaultValue -1
            let worst position = shape.WorstSuit |> Option.bind (fun (suit, _) -> card suit position)
            let minorsOfLength n =
                [ Suit.Clubs; Suit.Diamonds ] |> List.filter (fun s -> length s = n) |> List.length
            let smallestMinor =
                let c, d = length Suit.Clubs, length Suit.Diamonds
                if c + d = 0 then None
                elif c = 0 then Some Suit.Diamonds
                elif d = 0 then Some Suit.Clubs
                elif c > d then Some Suit.Diamonds
                elif c < d then Some Suit.Clubs
                else minorOfLength d
            let highestMinorLowEnd =
                let c, d = cards Suit.Clubs |> Array.rev, cards Suit.Diamonds |> Array.rev
                match Array.zip (Array.truncate d.Length c) (Array.truncate c.Length d)
                        |> Array.tryFind (fun (x, y) -> rankOf x <> rankOf y) with
                    | Some (x, y) -> Some (if rankOf x > rankOf y then Suit.Clubs else Suit.Diamonds)
                    | None when c.Length = d.Length -> None
                    | None -> Some (if c.Length > d.Length then Suit.Clubs else Suit.Diamonds)
            let highestNonSpade =
                [ Suit.Hearts; Suit.Clubs; Suit.Diamonds ]
                    |> List.filter (fun s -> length s > 0)
                    |> List.fold (fun best s ->
                        match best with
                            | Some b when high s < high b -> best
                            | _ -> Some s) None
            let minorWithLosers n maxLosers =
                minorOfLength n
                    |> Option.filter (fun s -> losers s <= maxLosers)
                    |> Option.bind (fun s -> card s 0)
            let secondHighHeartIf condition = card Suit.Hearts (if condition then 1 else 0)

                // each rule's card, if it applies, with whether it's a club or diamond singleton
            let rules : (unit -> Option<Card * bool>) list =
                let just f = fun () -> f () |> Option.map (fun c -> c, false)
                let singleton f = fun () -> f () |> Option.map (fun c -> c, true)
                [
                        // spades: the Q♠ without enough protection, and the A♠/K♠ in a short suit
                    just (fun () -> if holdingQS && nSpades <= 3 then card Suit.Spades qsPosition.Value else None)
                    just (fun () ->
                        if holdingQS && nSpades = 4 && progress.Direction = ExchangeDirection.Right then
                            card Suit.Spades qsPosition.Value
                        else None)
                    just (fun () -> if highSpades > 0 && nSpades = highSpades then card Suit.Spades 0 else None)
                    just (fun () -> if highSpades = 2 && nSpades = 3 && not passedQSRight then card Suit.Spades 0 else None)
                    just (fun () -> if highSpades > 0 && nSpades <= 2 && passedQSLeft then card Suit.Spades 0 else None)
                    just (fun () -> if not holdingQS && nSpades <= 2 && smallClubs then card Suit.Clubs 0 else None)
                    just (fun () ->
                        if not holdingQS && not passedQSRight && highSpades > 0 && nSpades <= highSpades + 2
                            && length Suit.Clubs > 0 then
                            card Suit.Spades 0
                        else None)

                        // voids, high hearts, and the high cards of a suit with low ones
                    singleton (fun () -> if minorOfLength 1 = Some Suit.Diamonds then card Suit.Diamonds 0 else None)
                    singleton (fun () -> if length Suit.Diamonds = 1 then card Suit.Diamonds 0 else None)
                    just (fun () -> if nHearts = 1 && highHeart >= 5 then card Suit.Hearts 0 else None)
                    just (fun () -> if dangerOfDeal >= 15 then worst 0 else None)
                    just (fun () ->
                        if not holdingQS && highSpades > 0 && not progress.PassedQS && nSpades <= 4 && smallClubs then
                            card Suit.Clubs 0
                        else None)
                    just (fun () -> minorWithLosers 2 0)
                    just (fun () -> if nHearts = 2 && highHeart >= 10 then card Suit.Hearts 0 else None)
                    just (fun () -> minorWithLosers 3 0)
                    just (fun () -> minorWithLosers 4 0)
                    just (fun () ->
                        if nHearts <= 5 && nHearts > run Suit.Hearts && run Suit.Hearts <= 2 && losers Suit.Hearts = 0 then
                            card Suit.Hearts 0
                        else None)
                    just (fun () -> minorWithLosers 2 1)
                    just (fun () -> minorWithLosers 3 1)
                    just (fun () -> minorWithLosers 4 1)

                        // from here, the least good card rather than the worst
                    just (fun () ->
                        if nHearts = 1 && (minorsOfLength 0 >= 1 || nSpades = 0) then card Suit.Hearts 0 else None)
                    just (fun () -> if minorOfLength 2 = Some Suit.Clubs && low Suit.Clubs >= 5 then card Suit.Clubs 0 else None)
                    just (fun () ->
                        if minorOfLength 2 = Some Suit.Diamonds && low Suit.Diamonds >= 5 then
                            if length Suit.Clubs = 2 && low Suit.Clubs > low Suit.Diamonds then card Suit.Clubs 0
                            else card Suit.Diamonds 0
                        else None)
                    just (fun () ->
                        if minorOfLength 2 = Some Suit.Diamonds && length Suit.Clubs = 2 && low Suit.Clubs >= 5 then
                            card Suit.Clubs 0
                        else None)
                    just (fun () -> if nHearts = 1 && low Suit.Hearts >= 5 then card Suit.Hearts 0 else None)
                    just (fun () -> if dangerOfDeal >= 10 then worst 0 else None)
                    just (fun () ->
                        minorOfLength 5
                            |> Option.filter (fun s -> losers s < 2)
                            |> Option.bind (fun s ->
                                card s (if holdingQS && nSpades <= 4 && high s >= queen then 2 else 0)))
                    just (fun () ->
                        if holdingQS && progress.Direction <> ExchangeDirection.Left && highSpades > 0
                            && nSpades - highSpades >= 5 then
                            card Suit.Spades 0
                        else None)
                    just (fun () ->
                        if beatableHeart && nHearts >= 2 && run Suit.Hearts <= 2 then
                            let gap = highHeart - rankOf ((cards Suit.Hearts)[1])
                            secondHighHeartIf (nHearts >= 5 || (nHearts = 4 && gap <= 2) || (nHearts = 3 && gap = 1))
                        else None)
                    just (fun () ->
                        match smallestMinor with
                            | Some s when nSpades <= 1 && length s <= 4 - progress.Step && not passedQSRight -> card s 0
                            | _ -> None)
                    just (fun () ->
                        if not beatableHeart && run Suit.Hearts = 0
                            && ((not progress.PassedHeart && highHeart > 5) || highHeart > 7) then
                            secondHighHeartIf (nHearts >= 5)
                        else None)
                    just (fun () ->
                        if not beatableHeart && run Suit.Hearts = 0 && nHearts = 1
                            && (losers Suit.Hearts = 0 || shape.Goren >= 17) then
                            card Suit.Hearts 0
                        else None)
                    fun () ->
                        highestMinorLowEnd
                            |> Option.filter (fun s -> high s >= 5)
                            |> Option.bind (fun s ->
                                card s (if length s >= 5 then 1 else 0) |> Option.map (fun c -> c, length s = 1))
                    just (fun () ->
                        highestNonSpade |> Option.bind (fun s ->
                            let second = (s = Suit.Hearts && beatableHeart && nHearts > 2) || length s >= 6
                            card s (if second then 1 else 0)))
                    just (fun () -> card Suit.Spades (nSpades - 1))
                ]
            rules |> List.pick (fun rule -> rule ())

        /// Killer Hearts' three-card pass to lie low, from the given dealt hand.
        let choose (dealt : Card[]) (direction : ExchangeDirection) =
            let rec loop step (hand : Card[]) (passed : Card list) (singletonStep : Option<int>) progress =
                if step > 3 then List.rev passed, hand, singletonStep
                else
                    let card, isSingleton = chooseNext (createShape hand) { progress with Step = step }
                    let progress =
                        { progress with
                            PassedQS = progress.PassedQS || (card.Suit = Suit.Spades && rankOf card = queen)
                            PassedHeart = progress.PassedHeart || card.Suit = Suit.Hearts }
                    loop (step + 1) (hand |> Array.filter ((<>) card)) (card :: passed)
                        (if isSingleton then Some step else singletonStep) progress
            let start = { Step = 1; Direction = direction; PassedQS = false; PassedHeart = false }
            let passed, rest, singletonStep = loop 1 dealt [] None start

                // having passed a club or diamond singleton, pass the other instead if it's higher
            let shape = createShape rest
            match singletonStep, shape.MinorByLength.TryFind 1 with
                | Some step, Some suit ->
                    let other = shape.Cards[suit][0]
                    let mine = passed[step - 1]
                    if rankOf mine < rankOf other then
                        passed |> List.mapi (fun i c -> if i = step - 1 then other else c)
                    else passed
                | _ -> passed

    let private choosePass (infoSet : InformationSet) =
        let passed = infoSet.OutgoingPassOpt |> Option.defaultValue Set.empty
        let dealt = Seq.append infoSet.Hand passed |> Seq.toArray
        LieLow.choose dealt infoSet.Deal.ExchangeDirection
            |> List.find (passed.Contains >> not)

    // ============================================================
    // Leading
    // ============================================================

    /// Leads the lowest card of my shortest suit that's safe to lead: not a high singleton, not a
    /// suit an opponent is void in, and, once points are split, not a suit whose lowest card is
    /// sure to win, since that wins a trick I needn't and may collect points dumped on it.
    let private leadFromShortestSafe sit (cards : Card[]) =
        let grouped =
            cards
                |> Array.groupBy (fun c -> c.Suit)
                |> Array.sortBy (fun (_, suitCards) -> suitCards.Length)

        let isSingletonHigh (suitCards : Card[]) =
            suitCards.Length = 1 && (lowest suitCards).Rank >= Rank.King

        let leadWins (suitCards : Card[]) =
            not sit.MoonOpen && isTopCard sit (lowest suitCards)

        let isSafeLead (suit, suitCards) =
            not (isSingletonHigh suitCards) && not (anyOpponentVoid sit suit) && not (leadWins suitCards)

        let isTolerableLead (_, suitCards) =
            not (isSingletonHigh suitCards) && not (leadWins suitCards)

        match grouped |> Array.tryFind isSafeLead with
            | Some (_, suitCards) -> lowest suitCards
            | None ->
                match grouped |> Array.tryFind isTolerableLead with
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

    /// With every point taken so far, and the Q♠ gone: lead my highest heart to keep taking them.
    let private tryLeadFishing sit =
        let hearts = sit.Legal |> bySuit Suit.Hearts
        if sit.QSPlayed && sit.MoonOpen && sit.Deal.Score[sit.Me] > 0 && hearts.Length > 0 then
            Some (highest hearts)
        else None

    /// Holding the Q♠: clear my shortest minor suit, high card first, to make a void for dumping it.
    let private tryLeadToVoid sit =
        let holdingQS = sit.Hand |> Array.exists isQS
        if holdingQS then
            sit.Legal
                |> Array.filter isMinor
                |> Array.groupBy (fun c -> c.Suit)
                |> Array.filter (fun (suit, _) -> (sit.Hand |> bySuit suit).Length <= 2)
                |> Array.sortBy (snd >> Array.length)
                |> Array.tryHead
                |> Option.map (snd >> highest)
        else None

    let private chooseLeadNormal sit =
        let holdingQS = sit.Hand |> Array.exists isQS
        let hasHighSpades =
            sit.Hand |> Array.exists (fun c -> c.Suit = Suit.Spades && c.Rank > Rank.Queen)
        let lowSpades = sit.Legal |> bySuit Suit.Spades |> belowRank Rank.Queen

        if not sit.QSPlayed then
            match tryLeadToVoid sit with
                | Some card -> card
                | None ->
                    if holdingQS || hasHighSpades then
                        let nonSpades = sit.Legal |> Array.filter (fun c -> c.Suit <> Suit.Spades)
                        if nonSpades.Length > 0 then leadFromShortestSafe sit nonSpades
                        else lowest sit.Legal
                    elif lowSpades.Length > 0 then
                        highest lowSpades   // flush out the Q♠, keeping my low spades for later
                    else leadFromShortestSafe sit sit.Legal
        else
            let nonQS = excludingQS sit.Legal
            leadFromShortestSafe sit (if nonQS.Length > 0 then nonQS else sit.Legal)

    let private chooseLead sit =
        if isShooting sit then
            chooseLeadShooting sit
        else
            tryLeadAgainstThreat sit
                |> Option.orElseWith (fun () -> tryLeadFishing sit)
                |> Option.defaultWith (fun () -> chooseLeadNormal sit)

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

    /// Against a probable shooter, following suit in a trick without the Q♠ at stake: win a
    /// trick the shooter led, with my lowest winner; otherwise duck as high as I can. When I
    /// can't win, keep my highest card as a stopper.
    let private tryStopShoot sit (trick : Trick) (cardsInSuit : Card[]) =
        match sit.ThreatOpt with
            | Some (shooter, _) ->
                let qsAtStake =
                    (cardsInSuit[0].Suit = Suit.Spades && not sit.QSPlayed)
                        || trick.Cards |> List.exists isQS
                if qsAtStake then None
                else
                    let _, winningCard = trick.HighPlayOpt.Value
                    let winners = cardsInSuit |> aboveRank winningCard.Rank
                    let losers = cardsInSuit |> belowRank winningCard.Rank
                    let byRank = cardsInSuit |> Array.sortByDescending (fun c -> c.Rank)
                    if losers.Length = 0 then Some (highest cardsInSuit)   // must win: go high
                    elif winners.Length > 0 then
                        if trick.Leader = shooter then Some (lowest winners)
                        else Some (highest losers)
                    elif byRank.Length >= 2 then Some byRank[1]
                    else Some byRank[0]
            | None -> None

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
            Some (if sure.Length > 0 then lowest sure else lowest winners)   // keep my higher cards as stoppers
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
        let noPointsTaken = sit.Deal.Score.Points |> Array.forall ((=) 0)
        let top = highest cardsInSuit

            // on a hold hand nobody passed into a void, so the early rounds of a suit are safe
        let isSafeEarlyRound () =
            sit.Deal.ExchangeDirection = ExchangeDirection.Hold
                && noPointsTaken
                && trickPoints = 0
                && isMinor top
                && not (anyOpponentVoid sit suitLed)
                && (sit.Deal.CompletedTricks |> List.filter (fun t -> t.SuitLedOpt = Some suitLed)).Length <= 1

            // with the Q♠ out but not mine, and no higher spades of my own to fear it
        let spadeBelowQueen () =
            let belowQueen = cardsInSuit |> belowRank Rank.Queen
            if suitLed = Suit.Spades && not sit.QSPlayed
                && not (sit.Hand |> Array.exists (fun c -> c.Suit = Suit.Spades && c.Rank >= Rank.Queen))
                && (belowQueen |> aboveRank winningCard.Rank).Length > 0 then
                Some (highest belowQueen)
            else None

        match cardsInSuit |> Array.tryFind isQS with
            | Some qs when winningCard.Rank > Rank.Queen && not onEdge ->
                qs   // dump the Q♠ under a higher spade
            | _ when onEdge && trickPoints = 0 ->
                    // nothing at stake: shed a middle card, keeping my high cards for control and
                    // my low ones as exits in case I have to abandon the moon
                if belowWinner.Length > 0 then highest belowWinner
                else lowestPreferNonQS cardsInSuit
            | _ when isLast ->
                if trickPoints = 0 && sit.QSPlayed && not sit.MoonOpen then
                        // nothing to gain by taking the lead
                    if belowWinner.Length > 0 then highest belowWinner
                    else highestPreferNonQS cardsInSuit
                elif trickPoints = 0 then highestPreferNonQS cardsInSuit   // free to shed a high card
                elif belowWinner.Length > 0 then highest belowWinner
                else highestPreferNonQS cardsInSuit
            | _ when top.Rank > winningCard.Rank && isSafeEarlyRound () ->
                top   // shed a high card while everyone must follow
            | _ ->
                match spadeBelowQueen () with
                    | Some card -> card
                    | None ->
                        if belowWinner.Length > 0 then
                                // duck as high as possible, under the Q♠'s rank while it's out
                            if suitLed = Suit.Spades && not sit.QSPlayed then
                                let belowQueen = belowWinner |> belowRank Rank.Queen
                                if belowQueen.Length > 0 then highest belowQueen
                                else highest belowWinner
                            else highest belowWinner
                        else
                                // I'm winning for now. If nobody can overtake me, or nothing is at
                                // stake, shed my highest card.
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

    /// A short suit whose every card would win a trick: a liability, best shed while I can.
    let private tryDiscardLiability sit =
        sit.Legal
            |> Array.groupBy (fun c -> c.Suit)
            |> Array.filter (fun (suit, cards) ->
                suit <> Suit.Hearts
                    && (suit <> Suit.Spades || sit.QSPlayed)
                    && cards.Length <= 2
                    && cards |> Array.forall (isLoser sit >> not))
            |> Array.sortBy (snd >> Array.length)
            |> Array.tryHead
            |> Option.map (snd >> highest)

    /// While the Q♠ is out, a high club or diamond could win a trick it's dumped on: shed the
    /// highest card of a short one.
    let private tryDiscardShortMinor sit =
        if sit.QSPlayed then None
        else
            sit.Legal
                |> Array.filter isMinor
                |> Array.groupBy (fun c -> c.Suit)
                |> Array.filter (fun (_, cards) -> cards.Length <= 2)
                |> Array.sortBy (snd >> Array.length)
                |> Array.tryHead
                |> Option.map (snd >> highest)

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
                    else
                        tryDiscardLiability sit
                            |> Option.orElseWith (fun () -> tryDiscardShortMinor sit)
                            |> Option.defaultWith (fun () ->
                                if hearts.Length > 0 then highest hearts
                                else safeDiscard sit sit.Legal)

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
            tryStopShoot sit trick cardsInSuit
                |> Option.orElseWith (fun () -> tryTake sit trick cardsInSuit)
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
