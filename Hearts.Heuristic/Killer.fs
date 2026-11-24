namespace Hearts.Heuristic

open PlayingCards
open Hearts

/// Killer Hearts V8: Gerald Berns' rule-based Hearts program, after its last VB6 version (V7.2), playing
/// at the game score under the information set's rules.
///
/// - Passing: Killer Hearts' pass (see KillerEngine/Pass.fs). It passes to shoot the moon when the hand left
///   after a trial pass to shoot is strong enough, by a threshold that falls when opponents would help a
///   Shoot or the player trails far enough to need one; otherwise it passes to stick the low scorer or
///   to lie low, as the game score suggests. (At zero game scores a more cautious pass did better, but
///   with real scores this bolder one wins more games.)
/// - Playing: Killer Hearts' play, ported exactly (see KillerEngine/). It infers what each opponent holds,
///   watches for a Shoot, chooses a mode of play from the game score and the deal (shoot, stop a Shoot,
///   help a Shoot, get the low scorer, protect the high scorer, lie low, ...), and picks the card its
///   script for that mode calls for.
/// - Shooting: never, when a Shoot would lose the game under the rules (see shootLoses). Killer Hearts
///   assumes its own rule, under which the Shooter may subtract 26 from its own score instead. Under the
///   arena's rule, which always adds 26 to the others, a Shoot that ends the game can leave an opponent
///   below the Shooter, so then the player neither passes nor plays to Shoot, but plays one of its
///   defensive modes instead, which avoid taking points; and once it has taken every point played so
///   far, it hands an opponent a point when it can (see breakMoon).
/// - Shooting more readily when behind: it needs less strength to try to Shoot when it trails the low
///   score by 10 or more (see trailingShootShift).
/// - Quirks: it plays V7.2, which fixed five slips in V7.1's code. They came up rarely, and fixing them
///   made no measurable difference.
module Killer =

    /// Would a Shoot by the information set's player end the game with someone else winning it, under the
    /// information set's rules?
    let shootLoses (infoSet : InformationSet) =
        let me = infoSet.Player
        let rules = infoSet.Rules
        let gameScore = infoSet.GameScore
        let final =
            let toOthers =
                Enum.getValues<Seat>
                    |> Array.map (fun seat ->
                        if seat = me then 0 else ClosedDeal.numPointsPerDeal)
                    |> Score.ofPoints
            let added = gameScore + toOthers
            match rules.MoonShotScoring with
                | AlwaysAddToOthers -> added
                | AllowSubtraction ->
                    if (GameScore.findGameLeaders added).Contains(me) then added
                    else gameScore + Score.create me -ClosedDeal.numPointsPerDeal
        let winners = GameScore.findGameWinners rules.TieGame final
        not winners.IsEmpty && not (winners.Contains(me))

    /// When the information set's player has taken every point played so far, a card that keeps it from
    /// taking them all. It ducks the trick if it can, or else plays low for a later player to beat; sluffs a
    /// point card onto an opponent's trick; and leads the card most likely to lose. When every card would win,
    /// it plays its surest winner, keeping its low cards to lose a later trick. None if an opponent has taken a point already.
    let breakMoon (infoSet : InformationSet) =
        let deal = infoSet.Deal
        let taken = deal.Score
        let total = Score.sum taken
        if total = 0 || taken[infoSet.Player] <> total then None
        else
            let legal = infoSet.LegalActions

                // the unplayed cards of a card's suit, held by opponents, that beat it
            let higherOut (card : Card) =
                deal.UnplayedCards
                    |> Seq.filter (fun c -> c.Suit = card.Suit && c.Rank > card.Rank && not (infoSet.Hand.Contains c))
                    |> Seq.length
            let surestWinner () = legal |> Array.sortBy (fun card -> higherOut card, -int card.Rank) |> Array.head

            match deal.CurrentTrickOpt with
                | Some trick when not trick.Cards.IsEmpty ->
                    let _, high = trick.HighPlayOpt.Value
                    if legal |> Array.exists (fun card -> card.Suit = high.Suit) then
                        let ducks = legal |> Array.filter (fun card -> card.Rank < high.Rank)
                        if ducks.Length > 0 then Some (ducks |> Array.maxBy _.Rank)   // lose the trick
                        elif trick.Cards.Length = Seat.numSeats - 1 then Some (legal |> Array.maxBy _.Rank)   // last to play: win with the highest, keeping the low cards
                        else Some (legal |> Array.minBy _.Rank)   // a later player may beat it
                    else
                        match legal |> Array.filter (fun card -> Card.pointValue card > 0) with
                            | [||] -> Some (surestWinner ())
                            | points -> Some (points |> Array.maxBy (fun (card : Card) -> Card.pointValue card, card.Rank))
                | _ ->
                    let best = legal |> Array.maxBy higherOut
                    if higherOut best = 0 then Some (surestWinner ())
                    else
                        legal
                            |> Array.sortByDescending (fun card -> higherOut card, (if card.Suit = Suit.Hearts then 1 else 0), -int card.Rank)
                            |> Array.tryHead

    /// How far the information set's player trails the game's low score.
    let deficit (infoSet : InformationSet) =
        let points = infoSet.GameScore.Points
        points[int infoSet.Player] - Array.min points

    /// The shift in the strength Killer Hearts V8 needs to try to Shoot (see Options.ShootShift): 6 less when it
    /// trails the low score by 10 or more. Under the arena's rule a moon adds 26 to every opponent, so it
    /// is the trailing player's best way back. (Tested: shifts of 4 to 6 from a deficit of 5 to 10 did
    /// best, about +0.004 game win rate against both itself without the shift and Killer Hearts V7.1;
    /// shifting at every score, either way, did worse.)
    let trailingShootShift infoSet = if deficit infoSet >= 10 then -6 else 0

    /// Killer Hearts V8, shifting the strength it needs to try to Shoot by the given function of the
    /// information set (see Options.ShootShift).
    let createWith (shootShift : InformationSet -> int) =
        let act (infoSet : InformationSet) =
            let noShoot = shootLoses infoSet
            let options : KillerEngine.Basics.Options =
                { NoShoot = noShoot; ShootShift = shootShift infoSet }
            match infoSet.LegalActionType with
                | ActionType.Pass ->
                    let daring = if noShoot then KillerEngine.Pass.Never else KillerEngine.Pass.Bold
                    KillerEngine.Pass.passWith options daring infoSet
                | ActionType.Play ->
                    let card = KillerEngine.Play.playWith options infoSet
                    if noShoot then defaultArg (breakMoon infoSet) card
                    else card
        { Act = act }

    /// Killer Hearts V8.
    let player = createWith trailingShootShift
