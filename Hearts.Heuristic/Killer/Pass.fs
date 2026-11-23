namespace Hearts.Heuristic.Killer

open PlayingCards
open Hearts

/// Killer Hearts' pass, ported.
///
/// Killer Hearts first decides what kind of pass to make. It passes to shoot the moon when the hand
/// left after a trial pass to shoot is strong enough; to stick the low scorer with Q♠ when it sits
/// right behind the low scorer, far enough ahead of it; and otherwise to lie low. Then it chooses the
/// three cards one at a time, by an ordered list of rules re-evaluated on the remaining hand after each
/// choice.
///
/// A cautious option passes to shoot only from the strongest of those hands.
module Pass =

    let private rankOf (card : Card) = int card.Rank

    /// Tricks a suit is expected to take, by the number of its cards the opponents hold.
    let private estimatedTricks = [| 0; 1; 1; 2; 3; 3; 3; 4; 4; 5; 6; 6; 6; 6 |]

    /// What the pass needs to know of the game score (set_initial_parameters,
    /// determine_hi_and_low_scorers).
    type private Scores =
        {
            /// Each seat's game score, by Killer Hearts seat.
            Score : int[]
            Me : KSeat
            Receiver : KSeat
            LowScore : int
            SecondLowestScore : int
            HighScore : int
            LowScorer : KSeat

            /// How far the seat trails the low score.
            Deficit : int

            /// How far the high score is from 100.
            Ceiling : int

            /// How far the seat's score is from 100.
            Surplus : int

            /// Does the receiver of the pass make a target: is the seat the low scorer, or the
            /// receiver near it?
            ReceiverIsTarget : bool

            /// Opponents for whom the seat's Shoot would do more good than harm.
            ShootHelpers : int

            /// The Goren count at which a hand is strong enough to try to shoot, lower when opponents
            /// would help a Shoot, or the seat trails far enough to need one.
            Threshold : int
        }

    let private createScores (score : int[]) (me : KSeat) (direction : int) =
        let receiver = passTarget direction me
        let mutable low, second, high, lowScorer = 200, 0, -200, none
        let mutable p = me
        for _ in 1 .. 4 do
            if score[p] < low then
                second <- low
                low <- score[p]
                lowScorer <- p
            elif score[p] < second then second <- score[p]
            if score[p] > high then high <- score[p]
            p <- nextSeat p
        let deficit = score[me] - low
        let ceiling = 100 - high
        let surplus = 100 - score[me]
        let helpers =
            [ 1 .. 3 ] |> List.filter (fun k ->
                let p = (me + k) % 4
                if score[p] = low then
                    second - low >= 20 && score[me] - 26 >= low + 10 && (score[me] < high || surplus > 25)
                else score[me] - 26 >= low)
                |> List.length
        let threshold =
            let t = if helpers <= 1 then 24 elif helpers = 2 then 21 else 18
            let t = if ceiling <= 34 && deficit >= 15 then t - (34 - ceiling) * (deficit + 20) / 398 else t
            t - 3 * deficit / 65
        {
            Score = score
            Me = me
            Receiver = receiver
            LowScore = low
            SecondLowestScore = second
            HighScore = high
            LowScorer = lowScorer
            Deficit = deficit
            Ceiling = ceiling
            Surplus = surplus
            ReceiverIsTarget = score[me] = low || score[receiver] <= low + 10
            ShootHelpers = helpers
            Threshold = threshold
        }

    /// Is the receiver of the pass far enough from the low score to be passed Q♠?
    let private receiverWellAboveLow (scores : Scores) =
        scores.Score[scores.Receiver] - scores.LowScore > 5

    /// Number of cards at the top of a suit (highest first) that run down from the ace.
    let private topRun (cards : Card[]) =
        cards
            |> Seq.indexed
            |> Seq.takeWhile (fun (i, c) -> rankOf c = ace - i)
            |> Seq.length

    /// Tricks a suit (highest first) takes when its holder leads it: all of them, if its top run is
    /// long enough to draw the opponents' cards, else just the top run.
    let private offensiveWinners (cards : Card[]) =
        let run = topRun cards
        if run > 0 && run >= estimatedTricks[13 - cards.Length] - 1 then cards.Length
        else run

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

    /// A suit's strength by Charles Goren's point count for bridge bidding: 4 for an ace down to 1 for a
    /// jack, counted only down an unbroken run of face cards; 1 to 3 for a void, singleton or doubleton;
    /// 1 to 3 for a suit of 5 to 7 cards headed by a face card; and 1 for the Q♠ with 4 other spades.
    let private gorenCount suit (cards : Card[]) =
        let shape =
            if cards.Length <= 2 then 3 - cards.Length
            elif cards.Length >= 5 && rankOf cards[0] >= jack then min cards.Length 7 - 4
            else 0
        let honors = cards |> Seq.takeWhile (fun c -> rankOf c >= jack) |> Seq.sumBy (fun c -> rankOf c - 10)
        let queenOfSpades =
            if suit = Suit.Spades && cards.Length >= 5 && cards |> Array.exists (fun c -> rankOf c = queen) then 1
            else 0
        shape + honors + queenOfSpades

    let private suits = [ Suit.Spades; Suit.Hearts; Suit.Clubs; Suit.Diamonds ]

    /// What the rules need to know about the hand that's left.
    type private Shape =
        {
            /// Each suit's cards, highest first.
            Cards : Map<Suit, Card[]>
            DefensiveLosers : Map<Suit, int>
            OffensiveWinners : Map<Suit, int>
            TotalOffensiveWinners : int
            TopRun : Map<Suit, int>

            /// Clubs or diamonds of each length; on a tie, the weaker of the two.
            MinorByLength : Map<int, Suit>

            /// Clubs or diamonds, whichever has the higher low cards (compared from the bottom up),
            /// or else the longer.
            MinorWithHigherLowEnd : Option<Suit>

            GorenBySuit : Map<Suit, int>
            Goren : int

            /// Clubs or diamonds, whichever is shorter, or on a tie the one MinorByLength names (smallest_CD).
            SmallestMinor : Option<Suit>

            /// The number of clubs and diamonds suits of each length, 0 and 1 (CD_suits_by_len).
            VoidMinors : int
            SingletonMinors : int

            /// The number of suits of length 5 (all_suits_by_len(5)).
            FiveCardSuits : int

            /// Clubs, diamonds or hearts, whichever has the highest low cards, and how high they are.
            WorstSuit : Option<Suit * int>

            /// The suit to pass from when shooting: a heart that won't win, or else the weakest suit
            /// (by Goren count, then fewest winners, then most cards) of those that won't take every
            /// trick, or else the longest.
            WeakestSuit : Suit
        }

    let private createShape (hand : Card[]) =
        let cards =
            suits
                |> List.map (fun suit ->
                    suit, hand |> Array.filter (fun c -> c.Suit = suit) |> Array.sortByDescending rankOf)
                |> Map
        let length suit = cards[suit].Length
        let losers = cards |> Map.map (fun _ cs -> defensiveLosers cs)
        let winners = cards |> Map.map (fun _ cs -> offensiveWinners cs)
        let goren = cards |> Map.map gorenCount
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
        let minorWithHigherLowEnd =
            let c, d = Array.rev clubs, Array.rev diamonds
            match Array.zip (Array.truncate d.Length c) (Array.truncate c.Length d)
                    |> Array.tryFind (fun (x, y) -> rankOf x <> rankOf y) with
                | Some (x, y) -> Some (if rankOf x > rankOf y then Suit.Clubs else Suit.Diamonds)
                | None when c.Length = d.Length -> None
                | None -> Some (if c.Length > d.Length then Suit.Clubs else Suit.Diamonds)
        let worstSuit =
            [ Suit.Hearts; Suit.Clubs; Suit.Diamonds ]
                |> List.map (fun suit ->
                    let low = cards[suit] |> Array.rev |> Array.truncate 3
                    suit, low |> Array.mapi (fun t c -> rankOf c - 2 - t) |> Array.sum)
                |> List.fold (fun best (suit, score) ->
                    match best with
                        | Some (_, top) when score <= top -> best
                        | _ -> Some (suit, score)) None
        let weakestSuit =
            let nHearts = length Suit.Hearts
            if nHearts > winners[Suit.Hearts] && nHearts < 5 then Suit.Hearts
            else
                let held = suits |> List.filter (fun s -> length s > 0)
                match held |> List.filter (fun s -> length s > winners[s]) with
                    | [] -> held |> List.maxBy length
                    | losing -> losing |> List.minBy (fun s -> goren[s], winners[s], -(length s))
        {
            Cards = cards
            DefensiveLosers = losers
            OffensiveWinners = winners
            TotalOffensiveWinners = winners |> Map.values |> Seq.sum
            TopRun = cards |> Map.map (fun _ cs -> topRun cs)
            MinorByLength = minorByLength
            MinorWithHigherLowEnd = minorWithHigherLowEnd
            GorenBySuit = goren
            Goren = goren |> Map.values |> Seq.sum
            WorstSuit = worstSuit
            WeakestSuit = weakestSuit
            SmallestMinor =
                let c, d = clubs.Length, diamonds.Length
                if c + d = 0 then None
                elif c = 0 then Some Suit.Diamonds
                elif d = 0 then Some Suit.Clubs
                elif c > d then Some Suit.Diamonds
                elif c < d then Some Suit.Clubs
                else minorByLength.TryFind d
            VoidMinors = [ clubs; diamonds ] |> List.filter (fun cs -> cs.Length = 0) |> List.length
            SingletonMinors = [ clubs; diamonds ] |> List.filter (fun cs -> cs.Length = 1) |> List.length
            FiveCardSuits = cards |> Map.values |> Seq.filter (fun cs -> cs.Length = 5) |> Seq.length
        }

    /// What's been decided so far in this pass.
    type private Progress =
        {
            Step : int
            Direction : ExchangeDirection
            PassedQS : bool
            PassedHeart : bool
        }

    /// The pass to lie low: it passes the Q♠ and the A♠/K♠ only from a short suit, empties short clubs
    /// and diamonds, and passes the high cards of suits whose low cards are dangerous, but keeps the
    /// high hearts and spades that protect a long suit.
    module private LieLowRules =

        /// The next card to pass, by the first rule that applies, with whether it's a club or diamond
        /// singleton.
        let chooseNext (scores : Scores) (shape : Shape) (progress : Progress) =
            let cards suit = shape.Cards[suit]
            let length suit = (cards suit).Length
            let high suit = if length suit = 0 then 0 else rankOf ((cards suit)[0])
            let low suit = if length suit = 0 then 0 else rankOf (Array.last (cards suit))
            let card suit position = Some ((cards suit)[position])
            let losers suit = shape.DefensiveLosers[suit]
            let run suit = shape.TopRun[suit]
            let minorOfLength n = shape.MinorByLength.TryFind n

            let spadeCards = cards Suit.Spades
            let nSpades = spadeCards.Length
            let qsPosition = spadeCards |> Array.tryFindIndex (fun c -> rankOf c = queen)
            let holdingQS = qsPosition.IsSome
            let highSpades =
                if nSpades >= 2 && rankOf spadeCards[1] = king then 2
                elif nSpades >= 1 && rankOf spadeCards[0] >= king then 1
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
                    just (fun () ->
                        // Q♠ heading four spades, to a receiver above the low score
                        if qsPosition = Some 0 && nSpades = 4 && receiverWellAboveLow scores
                            && progress.Direction <> ExchangeDirection.Left
                            && shape.VoidMinors + shape.SingletonMinors = 0 && rankOf spadeCards[1] < jack then
                            if nHearts < 5 then card Suit.Spades 0
                            else
                                match shape.SmallestMinor with
                                    | Some s when length s <= 4 -> card s 0
                                    | _ -> failwith "With QxxxS and xxxxxH, cannot find CD with length <= 3"
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
                        if holdingQS && scores.ReceiverIsTarget && progress.Direction <> ExchangeDirection.Left
                            && highSpades > 0 && nSpades - highSpades >= 5 then
                            card Suit.Spades 0
                        else None)
                    just (fun () ->
                        // far behind: pass every heart that won't win
                        if nHearts > run Suit.Hearts && nHearts - run Suit.Hearts <= 4 - progress.Step
                            && scores.Deficit >= 60 then
                            card Suit.Hearts (run Suit.Hearts)
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
                            && (losers Suit.Hearts = 0 || shape.Goren >= scores.Threshold - 7) then
                            card Suit.Hearts 0
                        else None)
                    just (fun () ->
                        // far behind with a strong hand and no hearts: keep the strong minor for a Shoot
                        if nHearts = 0 && scores.Deficit >= 30 && shape.Goren >= scores.Threshold - 7
                            && length Suit.Clubs > 0 && length Suit.Diamonds > 0 then
                            let gc, gd = shape.GorenBySuit[Suit.Clubs], shape.GorenBySuit[Suit.Diamonds]
                            if gc < gd || (gc = gd && length Suit.Clubs < length Suit.Diamonds) then card Suit.Clubs 0
                            else card Suit.Diamonds 0
                        else None)
                    fun () ->
                        shape.MinorWithHigherLowEnd
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

    /// The pass to stick the low scorer: from a seat that sits right behind the low scorer's receiver
    /// (the receiver sits behind the low scorer), it passes Q♠ when that is safe, so the receiver can
    /// drop it on the low scorer; otherwise much as the pass to lie low.
    module private GetLowScorerRules =

        /// A chooser of the next card to pass, by the first rule that applies, with whether it's a club or
        /// diamond singleton. It remembers whether it has passed Q♠ across the three cards.
        let create (fixQuirks : bool) (scores : Scores) (dealtHasQS : bool) =

            // pass Q♠ to the opponent sitting behind the low scorer: 0 no, 1 if opportune, 2 passed
            let mutable toBetterSeat =
                let j = (scores.Receiver + 3) % 4
                if not dealtHasQS || scores.HighScore < 13 then 0
                elif scores.Deficit <= 10 then 0   // the seat is too good a target itself
                elif scores.Score[scores.Receiver] = scores.LowScore then 0   // don't help the low scorer
                elif scores.Score[j] = scores.LowScore then 1
                else 0
            let mutable passedQS = false

            fun (shape : Shape) (progress : Progress) ->
                let cards suit = shape.Cards[suit]
                let length suit = (cards suit).Length
                let high suit = if length suit = 0 then 0 else rankOf ((cards suit)[0])
                let low suit = if length suit = 0 then 0 else rankOf (Array.last (cards suit))
                let card suit position = Some ((cards suit)[position])
                let losers suit = shape.DefensiveLosers[suit]
                let run suit = shape.TopRun[suit]
                let minorOfLength n = shape.MinorByLength.TryFind n

                let spadeCards = cards Suit.Spades
                let nSpades = spadeCards.Length
                let qsPosition = spadeCards |> Array.tryFindIndex (fun c -> rankOf c = queen)
                let holdingQS = qsPosition.IsSome
                let highSpades =
                    if nSpades >= 2 && rankOf spadeCards[1] = king then 2
                    elif nSpades >= 1 && rankOf spadeCards[0] >= king then 1
                    else 0
                let passedQSLeft = progress.PassedQS && progress.Direction = ExchangeDirection.Left
                let smallClubs = length Suit.Clubs > 0 && length Suit.Clubs <= 4 - progress.Step
                let nHearts = length Suit.Hearts
                let highHeart = high Suit.Hearts
                let beatableHeart = nHearts >= 16 - highHeart
                let dangerOfDeal = shape.WorstSuit |> Option.map snd |> Option.defaultValue -1
                let worst position = shape.WorstSuit |> Option.bind (fun (suit, _) -> card suit position)
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
                let qs () = card Suit.Spades qsPosition.Value

                    // each rule's card, if it applies, with whether it's a club or diamond singleton
                let rules : (unit -> Option<Card * bool>) list =
                    let just f = fun () -> f () |> Option.map (fun c -> c, false)
                    let singleton f = fun () -> f () |> Option.map (fun c -> c, true)
                    [
                            // spades: Q♠ with up to four spades, and the A♠/K♠ in a short suit
                        just (fun () ->
                            if holdingQS && nSpades <= 4 then
                                passedQS <- true
                                qs ()
                            else None)
                        just (fun () -> if highSpades > 0 && nSpades = highSpades then card Suit.Spades 0 else None)
                        just (fun () -> if highSpades = 2 && nSpades = 3 then card Suit.Spades 0 else None)
                        just (fun () -> if highSpades > 0 && nSpades <= 2 && passedQSLeft then card Suit.Spades 0 else None)
                        just (fun () ->
                            if not holdingQS && highSpades > 0 && not progress.PassedQS && nSpades <= 4 && smallClubs then
                                card Suit.Clubs 0
                            else None)
                        just (fun () -> if not holdingQS && nSpades <= 2 && smallClubs then card Suit.Clubs 0 else None)
                        just (fun () ->
                            if not holdingQS && highSpades > 0 && nSpades <= highSpades + 2 && length Suit.Clubs > 0 then
                                card Suit.Spades 0
                            else None)

                            // voids, high hearts, and the high cards of a suit with low ones
                        singleton (fun () ->
                            // Killer Hearts tests low_in(CD_by_len(1) >= 5), the low spade, for low_in(CD_by_len(1)) >= 5
                            match minorOfLength 1 with
                                | Some s when (if fixQuirks then low s >= 5 else low Suit.Spades <> 0) -> card s 0
                                | _ -> None)
                        singleton (fun () ->
                            match minorOfLength 1 with
                                | Some s when holdingQS && nSpades < 5 -> card s 0
                                | _ -> None)
                        just (fun () -> if nHearts = 1 && highHeart >= 5 then card Suit.Hearts 0 else None)
                        just (fun () -> minorWithLosers 2 0)
                        just (fun () -> minorWithLosers 3 0)
                        just (fun () -> minorWithLosers 4 0)
                        just (fun () ->
                            if nHearts <= 5 && nHearts > run Suit.Hearts && run Suit.Hearts <= 2 && losers Suit.Hearts = 0 then
                                card Suit.Hearts 0
                            else None)
                        just (fun () -> minorWithLosers 2 1)
                        just (fun () -> if dangerOfDeal >= 15 then worst 0 else None)
                        just (fun () -> minorWithLosers 3 1)
                        just (fun () -> minorWithLosers 4 1)

                            // Q♠ to the opponent sitting behind the low scorer
                        just (fun () ->
                            if toBetterSeat = 1 && holdingQS then
                                toBetterSeat <- 2
                                qs ()
                            else None)
                        just (fun () -> if toBetterSeat = 2 && nSpades > 0 then card Suit.Spades 0 else None)

                            // from here, the least good card rather than the worst
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
                        just (fun () ->
                            if holdingQS && progress.Step <= 2 then
                                passedQS <- true
                                qs ()
                            else None)
                        just (fun () -> if passedQS && nSpades > 0 then card Suit.Spades 0 else None)
                        just (fun () ->
                            minorOfLength 5
                                |> Option.filter (fun s -> losers s < 2)
                                |> Option.bind (fun s ->
                                    card s (if holdingQS && nSpades <= 4 && high s >= queen then 2 else 0)))
                        just (fun () ->
                            if holdingQS && scores.ReceiverIsTarget && progress.Direction <> ExchangeDirection.Left
                                && nSpades - highSpades >= 5 then
                                card Suit.Spades 0
                            else None)
                        just (fun () ->
                            if beatableHeart && not progress.PassedHeart && nHearts >= 2 then
                                let gap = highHeart - rankOf ((cards Suit.Hearts)[1])
                                secondHighHeartIf (nHearts >= 5 || (nHearts = 4 && gap <= 2) || (nHearts = 3 && gap = 1))
                            else None)
                        just (fun () ->
                            if not beatableHeart && run Suit.Hearts = 0
                                && ((not progress.PassedHeart && highHeart > 5) || highHeart > 7) then
                                secondHighHeartIf (nHearts >= 5)
                            else None)
                        just (fun () ->
                            if not beatableHeart && run Suit.Hearts = 0 && nHearts = 1
                                && (losers Suit.Hearts = 0 || shape.Goren >= scores.Threshold - 7) then
                                card Suit.Hearts 0
                            else None)
                        fun () ->
                            shape.MinorWithHigherLowEnd
                                |> Option.filter (fun s -> high s >= 5)
                                |> Option.bind (fun s -> card s 0 |> Option.map (fun c -> c, length s = 1))
                        singleton (fun () -> minorOfLength 1 |> Option.bind (fun s -> card s 0))
                        just (fun () ->
                            highestNonSpade |> Option.bind (fun s ->
                                let second = (s = Suit.Hearts && beatableHeart && nHearts > 2) || length s >= 6
                                card s (if second then 1 else 0)))
                        just (fun () -> card Suit.Spades (nSpades - 1))
                    ]
                rules |> List.pick (fun rule -> rule ())

    /// The pass to shoot the moon: it keeps the high cards, passing the low cards of the weakest suit,
    /// except that it passes a lone Q♠, a short Q♠ to a receiver well above the low score, and the
    /// A♠/K♠ from a short suit without it.
    module private ShootRules =

        /// The next card to pass, by the first rule that applies, with whether it's a club or diamond
        /// singleton.
        let chooseNext (scores : Scores) (shape : Shape) (progress : Progress) =
            let lowest suit = Array.last shape.Cards[suit]
            let spadeCards = shape.Cards[Suit.Spades]
            let nSpades = spadeCards.Length
            let qsPosition = spadeCards |> Array.tryFindIndex (fun c -> rankOf c = queen)
            let holdingQS = qsPosition.IsSome
            let passedQSRight = progress.PassedQS && progress.Direction = ExchangeDirection.Right
            let nHearts = shape.Cards[Suit.Hearts].Length
            let heartWinners = shape.OffensiveWinners[Suit.Hearts]
            let topHearts = shape.TopRun[Suit.Hearts]
            let gimme =
                heartWinners > topHearts
                    || topHearts >= 4
                    || (shape.TotalOffensiveWinners >= 9 && heartWinners = nHearts)

                // the lowest card of the shortest minor, if short enough
            let lowestOfSmallestMinor message =
                match shape.SmallestMinor with
                    | Some s when shape.Cards[s].Length <= 3 -> lowest s, shape.Cards[s].Length = 1
                    | _ -> failwith message

            let farAhead = receiverWellAboveLow scores || scores.Deficit >= 20
            if gimme then lowest shape.WeakestSuit, false
            elif holdingQS && nSpades = 1 then spadeCards[0], false
            elif holdingQS && nSpades = 2 && farAhead && shape.VoidMinors < 2 && nHearts >= 8 then
                lowestOfSmallestMinor "With QxS, does not find C and D void (STM)"
            elif holdingQS && nSpades = 2 && farAhead && shape.VoidMinors < 2 then spadeCards[qsPosition.Value], false
            elif qsPosition = Some 0 && nSpades = 3 && receiverWellAboveLow scores && shape.VoidMinors = 0 && nHearts >= 7 then
                lowestOfSmallestMinor "With QxxS, can't find CD with len <= 3 (STM)"
            elif qsPosition = Some 0 && nSpades = 3 && receiverWellAboveLow scores && shape.VoidMinors = 0 then
                spadeCards[0], false
            elif not holdingQS && not passedQSRight && nSpades > 0 && nSpades <= 3
                && rankOf spadeCards[0] >= king then
                spadeCards[0], false
            else
                match shape.MinorWithHigherLowEnd with
                    | Some minor when holdingQS && shape.WeakestSuit = Suit.Spades && nSpades <= 4 ->
                        lowest minor, false
                    | _ -> lowest shape.WeakestSuit, false

    /// Passes three cards, one at a time, each chosen by the given rules from the hand that's left.
    /// Answers the cards, the hand that's left, and the step at which the rules last passed a club or
    /// diamond singleton, if any.
    let private passThree (chooseNext : Shape -> Progress -> Card * bool) (dealt : Card[]) (direction : ExchangeDirection) =
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
        loop 1 dealt [] None start

    /// Does the hand that's left after a pass to shoot have winners everywhere: nine tricks to lead,
    /// with every heart among them?
    let private hasWinnersEverywhere (shape : Shape) =
        shape.TotalOffensiveWinners >= 9
            && shape.OffensiveWinners[Suit.Hearts] = shape.Cards[Suit.Hearts].Length

    /// Is the hand that's left after a pass to shoot strong enough to try, by Killer Hearts' test? It
    /// is if its hearts will run, or if it has enough strength (by Goren count) with hearts that are
    /// high or few. Opponents who would help a Shoot make less strength enough.
    let private isStrongForShooting (scores : Scores) (shape : Shape) =
        let threshold = scores.Threshold
        let helpers = scores.ShootHelpers
        let hearts = shape.Cards[Suit.Hearts]
        let heartWinners = shape.OffensiveWinners[Suit.Hearts]
        let topHearts = shape.TopRun[Suit.Hearts]
        let shortSuits = shape.Cards |> Map.values |> Seq.filter (fun cs -> cs.Length <= 1) |> Seq.length
        let strongLongSuit =
            suits |> List.exists (fun s -> shape.Cards[s].Length >= 5 && shape.GorenBySuit[s] >= 9)
        if heartWinners > topHearts then true   // long hearts that will run
        elif topHearts >= 3 then true   // high hearts
        elif hasWinnersEverywhere shape then true
        elif shape.Goren >= threshold then
            topHearts = hearts.Length || rankOf (Array.last hearts) >= queen
        elif topHearts < hearts.Length then false
        elif shape.Goren >= threshold - 2 && shortSuits >= 2 then true   // every heart, if any, is a winner
        elif shape.Goren >= threshold - 4 && helpers = 3 && shape.FiveCardSuits = 2 then true
        elif not strongLongSuit then false
        elif shape.Goren >= 15 && helpers >= 2 then true
        elif shape.Goren >= 16 && helpers >= 1 then true
        else shape.Goren >= 19 && helpers = 0

    /// How readily to pass to shoot the moon.
    type Daring =
        /// As readily as Killer Hearts, whenever the hand left after a pass to shoot is strong enough
        /// to try.
        | Bold

        /// Only when the hand left after a pass to shoot has winners everywhere. This did better than Bold
        /// at zero game scores, but worse with real ones.
        | Cautious

        /// Never (not Killer Hearts': see Claude.fs).
        | Never

    /// Each thread's last pass's main choices, for checking the port.
    let private lastBrief = new System.Threading.ThreadLocal<string>(fun () -> "")

    /// This thread's last pass's main choices, for checking the port.
    let lastExplanation () = lastBrief.Value

    /// Killer Hearts' kinds of pass.
    type private Kind =
        | ToShoot
        | ToGetLowScorer
        | ToLieLow

    /// The three-card pass from the given dealt hand, for the given seat at the given game score
    /// (by Killer Hearts seat, none 100 or more): Killer Hearts' pass to shoot the moon, if the hand is
    /// strong enough to try at the given daring; else its pass to stick the low scorer, if the seat
    /// sits right; else its pass to lie low (best_determine_kind_of_pass_to_make, determine_best_pass).
    let private chooseFor (options : Options) (daring : Daring) (score : int[]) (me : KSeat) (dealt : Card[]) (direction : ExchangeDirection) =
        let scores = createScores score me (int direction)

            // trial pass to shoot
        let shootPass, shootRest, shootSingleton =
            passThree (ShootRules.chooseNext scores) dealt direction
        let isStrongEnough =
            match daring with
                | Bold -> isStrongForShooting scores
                | Cautious -> hasWinnersEverywhere
                | Never -> fun _ -> false
            // Killer Hearts tests its spades after the trial pass, which it doesn't recompute for the dealt hand
        let trialSpades = (createShape (if options.FixQuirks then dealt else shootRest)).Cards[Suit.Spades]
        let trialHoldsQS = trialSpades |> Array.exists (fun c -> rankOf c = queen)
        let kind =
            if isStrongEnough (createShape shootRest) then ToShoot
            else
                let nSpades = trialSpades.Length
                let holdingQS = trialHoldsQS
                if scores.Receiver <> (scores.LowScorer + 1) % 4 then ToLieLow
                elif score[scores.Receiver] <= scores.LowScore + 10 then ToLieLow
                elif not holdingQS || nSpades > 4 then ToLieLow
                elif scores.Deficit <= 10 then ToLieLow
                else ToGetLowScorer
        let passed, rest, singletonStep =
            match kind with
                | ToShoot -> shootPass, shootRest, shootSingleton
                | ToGetLowScorer -> passThree (GetLowScorerRules.create options.FixQuirks scores trialHoldsQS) dealt direction
                | ToLieLow -> passThree (LieLowRules.chooseNext scores) dealt direction

            // Killer Hearts doesn't reset its record of a passed club or diamond singleton after the trial pass
        let kindNumber = match kind with ToShoot -> 1 | ToGetLowScorer -> 3 | ToLieLow -> 5
        lastBrief.Value <-
            $"kind={kindNumber} thr={scores.Threshold} helpers={scores.ShootHelpers} deficit={scores.Deficit} low={scores.LowScore}/{scores.LowScorer} second={scores.SecondLowestScore} high={scores.HighScore} target={scores.ReceiverIsTarget} single={defaultArg singletonStep 0}"
        let singletonStep =
            if singletonStep.IsSome || options.FixQuirks then singletonStep else shootSingleton

            // with a hand strong enough to Shoot later, keep the higher club or diamond singleton
        let shape = createShape rest
        let replace step (card : Card) = passed |> List.mapi (fun i c -> if i = step - 1 then card else c)
        match shape.MinorByLength.TryFind 1 with
            | Some suit ->
                let other = shape.Cards[suit][0]
                let keepHigher =
                    if kind <> ToShoot && shape.Goren >= scores.Threshold && scores.Deficit > 8 then
                        passed |> List.tryFindIndex (fun c -> c.Suit = suit && rankOf other < rankOf c)
                    else None
                match keepHigher, singletonStep with
                    | Some i, _ -> replace (i + 1) other
                    | None, Some step ->
                            // having passed a club or diamond singleton, pass the other instead if it's higher (or,
                            // to shoot, lower)
                        let mine = passed[step - 1]
                        if kind <> ToShoot && rankOf mine < rankOf other then replace step other
                        elif kind = ToShoot && rankOf mine > rankOf other then replace step other
                        else passed
                    | None, None -> passed
            | None -> passed

    /// The next card to pass for the information set, at the given daring, departing from Killer Hearts as the
    /// options say.
    let passWith (options : Options) (daring : Daring) (infoSet : InformationSet) =
        let passed = infoSet.OutgoingPassOpt |> Option.defaultValue Set.empty
        let dealt = Seq.append infoSet.Hand passed |> Seq.toArray
        let score = killerScores infoSet.GameScore.Points
        chooseFor options daring score (int infoSet.Player) dealt infoSet.Deal.ExchangeDirection
            |> List.find (passed.Contains >> not)

    /// Killer Hearts' next card to pass for the information set, at the given daring.
    let pass daring infoSet = passWith Options.faithful daring infoSet
