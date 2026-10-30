namespace KHearts;

/// <summary>A view of the engine's reasoning on its last decision, for checking the port.</summary>
public partial class KhEngine
{
    public string DebugState()
    {
        string Suits<T>(VbArray<T> a) => string.Join(",", Enumerable.Range(spades, 4).Select(s => a[s]));
        string Seats<T>(VbArray<T> a) => string.Join(",", Enumerable.Range(player_west, 4).Select(p => a[p]));
        return string.Join(" ",
            $"mode={mode} def={best_determine_mode_mode_type[best_determine_mode_def]} off={best_determine_mode_mode_type[best_determine_mode_off]}",
            $"suit_to_sluff={suit_to_sluff} spades_safe={spades_safe} S_still_safe={S_still_safe}",
            $"def_suits={def_suits[0]}:{def_suits[1]},{def_suits[2]},{def_suits[3]},{def_suits[4]}",
            $"def_winners={Suits(Playing_def_winners)} LU={Suits(LU)} est_tr={Suits(est_tr)}",
            $"opps_have={Suits(opps_have)} opps_void_in={Suits(opps_void_in)} case_suit={Suits(case_suit)}",
            $"QS_owner={QS_owner} opp_owns_QS={opp_owns_QS} QS_in_hand={Playing_QS_in_hand} AKS_in_hand={Playing_AKS_in_hand}",
            $"pts_sit={pts_sit} likely_T_winner={likely_T_winner} opp_target={Seats(opp_target)} entries={entries}",
            $"Shooter={Shooter} poss={poss_opp_Shoot} prob={prob_opp_Shoot} oppose={oppose_Shoot}",
            $"deficit={deficit} im_score={Seats(im_score)} lead_over_2nd_low={lead_over_2nd_low} me_target={me_target}",
            $"high_in={Suits(Playing_high_in)} outcard={suit_of_outcard}/{outcard_suit_count}",
            $"sit={sit} rank_to_sluff={rank_to_sluff} HnQS={HnQS} AKS={AKS} live={live_suits}/{a_live_suit}",
            $"suit_is_live={Suits(suit_is_live)} suit_stopper={Suits(suit_stopper)} can_hold_QS={can_hold_QS}");
    }

    /// <summary>Features of the last decision, as comma-separated values, for finding patterns.</summary>
    public static string FeatureHeader =>
        "mode,sit,suit_to_sluff,spades_safe,S_still_safe,QS_in_hand,AKS_in_hand,AKS,suit_len_S,opp_owns_QS,pts_sit," +
        "def_suits0,entries,live_suits,trick,seq,sluff_pts,can_hold_QS,a_live_suit,live_S,live_H,live_C,live_D,QS_played,suit_len_H,entries_clipped";

    public string Features() =>
        string.Join(",", mode, sit, suit_to_sluff, spades_safe, S_still_safe, Playing_QS_in_hand, Playing_AKS_in_hand, AKS,
            Playing_suit_len_S, opp_owns_QS, pts_sit, def_suits[0], entries, live_suits, trick, seq, sluff_pts, can_hold_QS,
            a_live_suit, suit_is_live[spades], suit_is_live[hearts], suit_is_live[clubs], suit_is_live[diamonds], QS_played,
            Playing_suit_len_H, entries_clipped);
}
