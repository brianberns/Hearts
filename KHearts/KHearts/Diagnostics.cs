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

    /// <summary>The engine's main choices on its last play decision, in the same form as the F# port's Brief.</summary>
    public string Brief()
    {
        string Seats<T>(VbArray<T> a) => string.Join(",", Enumerable.Range(player_west, 4).Select(p => a[p]));
        return string.Join(" ",
            $"mode={mode} def={best_determine_mode_mode_type[best_determine_mode_def]} off={best_determine_mode_mode_type[best_determine_mode_off]} sit={sit}",
            $"Shooter={Shooter} poss={poss_opp_Shoot} prob={prob_opp_Shoot} oppose={oppose_Shoot} wins={opp_Shoot_wins_game} helpers={Playing_Shoot_helpers}",
            $"im={Seats(im_score)} low={low_score} high={high_score} deficit={deficit} ltw={likely_T_winner} tgt={Seats(opp_target)}",
            $"hold={can_hold_QS} eat={eat_QS} take={take_points} safe={safe_card} sluff={suit_to_sluff}/{rank_to_sluff} lead={best_lead_suit}/{best_lead_rank}",
            $"thr={Playing_threshold} goren={mod_Goren_count} go={go_Shoot} quit={quit_Shoot} rel={relinquish_QS}",
            LeadDetail());
    }

    /// <summary>The leading computation's main intermediate results, in the same form as the F# port's LeadDetail.</summary>
    string LeadDetail()
    {
        string Suits(VbArray<int> a) => string.Join(",", Enumerable.Range(spades, 4).Select(s => a[s]));
        var order = string.Join(",", Enumerable.Range(1, number_of_leadable_suits).Select(i => risk_rank_of_suit[i]));
        return string.Join(" ",
            $"\n      len={Suits(suit_len)} HU={Suits(Playing_HU)} LU={Suits(LU)} est={Suits(est_tr)} opps={Suits(opps_have)}",
            $"\n      safe={spades_safe} pull={pull_spades} wlu={suit_with_LU} power={power_suit} lowrisk={suit_of_lowest_risk}",
            $"risk={Suits(best_compute_parameters_related_to_leading_risk_rank)} order={order} ss={suit_smallest}/{len_ss} snal={suit_not_all_losers} pri={priority}");
    }
    /// <summary>The engine's main choices on its last pass, in the same form as the F# port's pass explanation.</summary>
    public string BriefPass() =>
        $"kind={kind_of_pass} thr={Passing_threshold} helpers={Passing_Shoot_helpers} deficit={deficit} low={low_score}/{low_scorer} second={second_lowest_score} high={Passing_high_score} target={pass_recvr_is_target} single={CD_singleton_passed}";}
