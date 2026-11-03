namespace Hearts.Heuristic

open PlayingCards
open Hearts

/// A rule-based Hearts player that plays like Killer Hearts, Gerald Berns' Hearts program.
///
/// - Passing: Killer Hearts' pass, ported exactly (see Killer/Pass.fs), but more cautious. It passes
///   to shoot the moon only when the hand left after a trial pass to shoot has winners everywhere,
///   and otherwise to lie low. Killer Hearts' bolder tries pay against weak players but cost against
///   strong ones, including Killer Hearts itself.
/// - Playing: Killer Hearts' play, ported exactly (see Killer/). It infers what each opponent holds,
///   watches for a Shoot, chooses a mode of play (shoot, stop a Shoot, lie low, ...), and picks the
///   card its script for that mode calls for.
///
/// It doesn't see the game score, so it decides as Killer Hearts does when every score is zero.
module Claude =

    let player =
        let act (infoSet : InformationSet) =
            match infoSet.LegalActionType with
                | ActionType.Pass -> Killer.Pass.pass Killer.Pass.Cautious infoSet
                | ActionType.Play -> Killer.Play.play infoSet
        { Act = act }
