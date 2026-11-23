namespace Hearts.Heuristic

open PlayingCards
open Hearts

/// A rule-based Hearts player that plays like Killer Hearts, Gerald Berns' Hearts program, at the game
/// score, under the information set's rules.
///
/// - Passing: Killer Hearts' pass, ported exactly (see Killer/Pass.fs), but more cautious. It passes
///   to shoot the moon only when the hand left after a trial pass to shoot has winners everywhere,
///   and otherwise to stick the low scorer or to lie low, as the game score suggests. Killer Hearts'
///   bolder tries pay against weak players but cost against strong ones, including Killer Hearts itself.
/// - Playing: Killer Hearts' play, ported exactly (see Killer/). It infers what each opponent holds,
///   watches for a Shoot, chooses a mode of play from the game score and the deal (shoot, stop a Shoot,
///   help a Shoot, get the low scorer, protect the high scorer, lie low, ...), and picks the card its
///   script for that mode calls for.
/// - Shooting: never, when a Shoot would lose the game under the rules (see shootLoses). Killer Hearts
///   assumes its own rule, under which the Shooter may subtract 26 from its own score instead. Under the
///   arena's rule, which always adds 26 to the others, a Shoot that ends the game can leave an opponent
///   below the Shooter, so then the player neither passes nor plays to Shoot, but plays one of its
///   defensive modes instead, which avoid taking points; and once it has taken every point played so
///   far, it hands an opponent a point when it can (see breakMoon).
module Claude =

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

    /// When the information set's player has taken every point played so far, a card that keeps it
    /// from taking them all: one that loses the trick, a point card sluffed onto an opponent's trick,
    /// or a low heart led for an opponent to win. None if there isn't one, or if an opponent has taken a
    /// point already.
    let breakMoon (infoSet : InformationSet) =
        let deal = infoSet.Deal
        let taken = deal.Score
        let total = Score.sum taken
        if total = 0 || taken[infoSet.Player] <> total then None
        else
            let legal = infoSet.LegalActions
            match deal.CurrentTrickOpt with
                | Some trick when not trick.Cards.IsEmpty ->
                    let _, high = trick.HighPlayOpt.Value
                    if legal |> Array.exists (fun card -> card.Suit = high.Suit) then
                        legal
                            |> Array.filter (fun card -> card.Rank < high.Rank)   // lose the trick
                            |> Array.sortByDescending _.Rank
                            |> Array.tryHead
                    else
                        legal   // sluff Q♠, else the highest heart
                            |> Array.filter (fun card -> Card.pointValue card > 0)
                            |> Array.sortByDescending (fun card -> Card.pointValue card, card.Rank)
                            |> Array.tryHead
                | _ ->
                    legal
                        |> Array.filter (fun card -> card.Suit = Suit.Hearts)
                        |> Array.sortBy _.Rank
                        |> Array.tryHead

    let player =
        let act (infoSet : InformationSet) =
            let noShoot = shootLoses infoSet
            match infoSet.LegalActionType with
                | ActionType.Pass ->
                    let daring = if noShoot then Killer.Pass.Never else Killer.Pass.Cautious
                    Killer.Pass.pass daring infoSet
                | ActionType.Play ->
                    let card = Killer.Play.playWith noShoot infoSet
                    if noShoot then defaultArg (breakMoon infoSet) card
                    else card
        { Act = act }
