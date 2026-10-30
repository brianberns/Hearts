namespace KHearts;

/// <summary>
/// The game loop around Killer Hearts' engine: the state-keeping parts of KHearts.frm's form_load,
/// play_one_game_of_Hearts, deal_four_13_card_hands, handle_passing, pick_up_pass, play_hand and
/// play_trick, ported by hand without the user interface.
///
/// The table follows one deal. As in the original, the engine decides for every seat in turn, even
/// for a seat whose card comes from outside (a human, or a Host-Client opponent), and its shared state
/// evolves exactly as the original's would.
///
/// Cards are Killer Hearts card numbers: 4 * (rank - 2) + suit, with suits spades = 0, hearts = 1,
/// clubs = 2, diamonds = 3. Seats are west = 0, north = 1, east = 2, south = 3, in playing order.
/// </summary>
public partial class KhEngine
{
    /// <summary>The seat whose turn it is.</summary>
    public int CurrentPlayer => player;

    /// <summary>The trick number, 1 to 13; 0 before play starts.</summary>
    public int TrickNumber => trick;

    /// <summary>Cards played so far on the current trick.</summary>
    public int Seq => seq;

    /// <summary>True when the engine has just claimed the remaining tricks for the current player.</summary>
    public bool Claimed { get; private set; }

    /// <summary>Points taken so far in this deal, by seat.</summary>
    public int PointsInHand(int seat) => score_in_hand[seat];

    /// <summary>Total points played so far in this deal, completed tricks only.</summary>
    public int PointsPlayed => points_played;

    /// <summary>
    /// True once a trick has decided the game, so the original ends the hand there. Play may continue
    /// here, for Hearts plays every deal out.
    /// </summary>
    public bool GameIsOver { get; private set; }

    /// <summary>The mode the engine chose on its last decision, for diagnosis.</summary>
    public int Mode => mode;

    /// <summary>The suit led to the current trick, or illegal_value before the lead.</summary>
    public int SuitLed => suit_led;

    /// <summary>The cards a seat holds.</summary>
    public int[] HandOf(int seat)
    {
        var cards = new List<int>();
        for (var s = spades; s <= diamonds; s++)
            for (var r = 2; r <= ace; r++)
                if (hands[seat, r, s] == 1)
                {
                    var card = 0;
                    get_index_from_rank_and_suit(ref card, r, s);
                    cards.Add(card);
                }
        return [.. cards];
    }

    /// <summary>The cards the current player may play.</summary>
    public int[] LegalCards() =>
        [.. HandOf(player).Where(card => IsLegal(card / 4 + 2, card % 4))];

    public KhEngine()
    {
        initialize_tables();
    }

    /// <summary>The engine-related initialization from form_load and start_Hearts.</summary>
    void initialize_tables()
    {
        player_name[player_west] = "West"; player_name[player_north] = "North"; player_name[player_east] = "East";
        player_name[player_south] = "South";
        player_abbrev[player_west] = "W"; player_abbrev[player_north] = "N"; player_abbrev[player_east] = "E";
        player_abbrev[player_south] = "S";
        skill_names[best_player] = "best"; skill_names[better_player] = "very good";
        skill_names[good_player] = "good"; skill_names[new_player] = "new"; skill_names[random_player] = "random";
        for (var r = 2; r <= 9; r++)
            rank_abbrev[r] = r.ToString();
        rank_abbrev[10] = "T"; rank_abbrev[jack] = "J"; rank_abbrev[queen] = "Q"; rank_abbrev[king] = "K";
        rank_abbrev[ace] = "A";
        suit_name[spades] = "spades"; suit_name[hearts] = "hearts"; suit_name[clubs] = "clubs";
        suit_name[diamonds] = "diamonds";
        suit_abbrev[spades] = "S"; suit_abbrev[hearts] = "H"; suit_abbrev[clubs] = "C"; suit_abbrev[diamonds] = "D";
        pass_text[pass_left] = "left"; pass_text[pass_right] = "right"; pass_text[pass_across] = "across";
        pass_text[hold] = "(hold)";
        pass_abbrev[pass_left] = "L"; pass_abbrev[pass_right] = "R"; pass_abbrev[pass_across] = "A";
        pass_abbrev[hold] = "H";
        player_incr[pass_left] = 1; player_incr[pass_right] = 3; player_incr[pass_across] = 2;

        for (var n = 0; n <= 30; n++)
            bit_no_on[n] = 1 << n;                      // bit_no_on(n) = 2^n
        p_posn[player_west] = 1; p_posn[player_north] = 2; p_posn[player_east] = 4; p_posn[player_south] = 8;
        q_posn[1] = player_west; q_posn[2] = player_north; q_posn[3] = illegal_value; q_posn[4] = player_east;
        q_posn[5] = illegal_value; q_posn[6] = illegal_value; q_posn[7] = illegal_value; q_posn[8] = player_south;
        for (var n = 0; n <= 7; n++)
            power_10[n] = (int)Math.Pow(10, n);

        for (var p = player_west; p <= player_south; p++)
        {
            human_player[p] = false;
            play_skill[p] = best_player;
        }

        // est_t_p(opps_void_in(), opps_have()): estimated tricks for pass hand
        int[] tp0 = [0, 1, 1, 2, 3, 3, 3, 4, 4, 5, 6, 6, 6, 6];
        int[] tp1 = [0, 1, 2, 2, 3, 3, 4, 5, 6, 6, 6, 7, 7, 7];
        // est_t_h(opps_void_in(), opps_have()): estimated tricks for hold hand
        int[] th0 = [0, 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5, 6];
        int[] th1 = [0, 1, 1, 2, 3, 4, 4, 5, 5, 6, 6, 6, 7, 7];
        for (var n = 0; n <= 13; n++)
        {
            est_t_p[0, n] = tp0[n]; est_t_p[1, n] = tp1[n];
            est_t_h[0, n] = th0[n]; est_t_h[1, n] = th1[n];
        }

        // tatt (trick adjustment to threshold, used with mod_Goren_count)
        int[] t = [100, 99, 93, 89, 85, 80, 75, 70, 65, 60, 55, 51];
        for (var n = 1; n <= 12; n++)
            tatt[n] = t[n - 1];

        im_score[illegal_value] = 0;                    // from start_Hearts
        ui_mnuRules_2C_leads_Checked = true;            // the two of clubs leads, as in Hearts' engine
        game_num = 1;
    }

    // ------------------------------------------------------------------------------------------------
    // Dealing

    /// <summary>
    /// Starts a deal, as play_one_game_of_Hearts, deal_four_13_card_hands and handle_passing's preamble
    /// do. The hands are the cards dealt to each seat, before any pass. The game score is each seat's
    /// score before this deal.
    /// </summary>
    public void StartDeal(int dealerSeat, int passDirection, int[] gameScore, int[][] dealtHands, bool isFirstDeal)
    {
        if (gameScore.Length != 4 || dealtHands.Length != 4)
            throw new ArgumentException("Four seats are required");
        dealer = dealerSeat;
        pass_direction = passDirection;
        first_hand = isFirstDeal;
        deal_number = 0;
        passes_made = 0;
        Claimed = false;
        GameIsOver = false;
        for (player = player_west; player <= player_south; player++)
        {
            if (gameScore[player] < 0 || gameScore[player] > 125)
                throw new ArgumentOutOfRangeException(nameof(gameScore), "Scores must be 0 to 125");
            score[player] = gameScore[player];          // initialize scores
            score_in_game[player] = gameScore[player];
        }
        set_score_between_hands();

        twoC_leads = true;                              // mnuRules_2C_leads.Checked
        sluff_pts = false;

        private_info.Clear();                           // done here because it records passes
        hands.Clear();                                  // initialize the hands
        var dealt = 0;
        for (var p = player_west; p <= player_south; p++)
        {
            foreach (var card in dealtHands[p])
            {
                var r = 0;
                var s = 0;
                get_rank_and_suit_from_index(card, ref r, ref s);
                if (hands[player_west, r, s] + hands[player_north, r, s] + hands[player_east, r, s]
                        + hands[player_south, r, s] > 0)
                    diagnose_and_halt("Same card dealt twice");
                hands[p, r, s] = 1;                     // mark player's hand with card dealt
                private_info[p, (p + 1) % 4, r, s] = 0x20F; // player knows opps cannot have this card
                private_info[p, (p + 2) % 4, r, s] = 0x20F;
                private_info[p, (p + 3) % 4, r, s] = 0x20F;
                dealt++;
            }
        }
        if (dealt != 52)
            diagnose_and_halt("Deal does not have 52 cards");
        for (var p = player_west; p <= player_south; p++)
            hand_size[p] = 13;

        if (pass_direction == hold)
        {
            for (var i = player_west; i <= player_south; i++)
            {
                passed_AS[i] = illegal_value;
                passed_KS[i] = illegal_value;
                passed_QS[i] = illegal_value;
            }
            start_play();
        }
        else
        {
            for (var i = player_west; i <= player_south; i++)
            {
                p_p[i] = (i - player_incr[pass_direction] + 4) % 4; // opponent who passes to player i
                r_p[i] = (i + player_incr[pass_direction]) % 4;     // opponent player i passes to
            }
            passer = dealer;
            NextPasser();
        }
    }

    // ------------------------------------------------------------------------------------------------
    // Passing

    int passer;
    int passes_made;

    /// <summary>The seat whose turn it is to pass, or illegal_value when passing is done.</summary>
    public int Passer => passes_made < 4 ? passer : illegal_value;

    void NextPasser()
    {
        passer = get_next_player(passer);
        player = passer;
        passing_player = p_p[player];
        receiving_player = r_p[player];
        show_thinking = false;
        this_is_best_player = play_skill[player] == best_player;
        trick = 1;                                      // required for determine_3_card_pass
    }

    /// <summary>
    /// The engine's three-card pass for the seat whose turn it is to pass. As in Host-Client mode, the
    /// engine computes a pass only for its own seats: skip this for a seat whose pass comes from outside.
    /// </summary>
    public int[] ComputePass()
    {
        determine_3_card_pass();                        // sets p_rank(1-3), p_suit(1-3)
        var cards = new int[3];
        for (var i = 1; i <= 3; i++)
            get_index_from_rank_and_suit(ref cards[i - 1], p_rank[i], p_suit[i]);
        return cards;
    }

    /// <summary>
    /// Makes the passing seat's pass. If the engine did not compute it, records the passed high spades as
    /// handle_passing does for a Client's pass.
    /// </summary>
    public void ApplyPass(int[] cards, bool computedByEngine)
    {
        if (cards.Length != 3)
            throw new ArgumentException("A pass has three cards");
        for (var i = 1; i <= 3; i++)
        {
            get_rank_and_suit_from_index(cards[i - 1], ref p_rank[i], ref p_suit[i]);
            if (!computedByEngine)
            {
                passed_AS[player] = illegal_value;
                passed_KS[player] = illegal_value;
                passed_QS[player] = illegal_value;
                if (p_suit[i] != spades)
                {
                    // null
                }
                else if (p_rank[i] == ace) passed_AS[player] = receiving_player;
                else if (p_rank[i] == king) passed_KS[player] = receiving_player;
                else if (p_rank[i] == queen) passed_QS[player] = receiving_player;
            }
        }
        for (var i = 1; i <= 3; i++)
        {
            recd_p_rank[receiving_player, i] = p_rank[i];
            recd_p_suit[receiving_player, i] = p_suit[i];
            if (hands[player, p_rank[i], p_suit[i]] == 1)
                hands[player, p_rank[i], p_suit[i]] = 0;    // remove passed card from hand
            else
                diagnose_and_halt("Passed card not in hand");
        }
        passes_made++;
        if (passer == dealer)
        {
            pick_up_pass();
            update_private_info_with_pass_information();
            start_play();
        }
        else
            NextPasser();
    }

    /// <summary>Each player picks up the pass, as pick_up_pass does.</summary>
    void pick_up_pass()
    {
        player = dealer;
        do
        {
            player = get_next_player(player);
            for (var i = 1; i <= 3; i++)
            {
                var r = recd_p_rank[player, i];
                var s = recd_p_suit[player, i];
                if (hands[player, r, s] == 1)
                    diagnose_and_halt("Received card is already in hand");
                hands[player, r, s] = 1;
            }
        } while (player != dealer);
    }

    // ------------------------------------------------------------------------------------------------
    // Playing

    /// <summary>The start of play_hand.</summary>
    void start_play()
    {
        arc_poss_opp_Shoot.Clear(); arc_prob_opp_Shoot.Clear(); cards_played.Clear(); count_played.Clear();
        has_led_spades.Clear(); has_sluffed.Clear(); has_sluffed_AKS.Clear(); high_leads_per_suit.Clear();
        last_sluffed_trick.Clear(); player_sluffs.Clear(); public_info.Clear(); score_in_hand.Clear();
        suit_pulls.Clear(); suit_sluffs.Clear(); tricks_won_by.Clear(); @void.Clear(); void_in_suit.Clear();
        for (var p = player_west; p <= player_south; p++)
        {
            arc_Shooter[p] = illegal_value;
            if (hands[p, queen, spades] == 1)
                owns_QS = p;
            played_AKS[p] = false;
        }
        for (var s = spades; s <= diamonds; s++)
            pub_lowest_unplayed[s] = 2;
        all_winners = false;
        all_winners_type = 0;                           // >0: player has all winners left
        AKS_played = 0;
        can_lead_H = false;
        follows_with_AKS_on_first_pull = illegal_value;
        follows_with_QS_on_first_pull = illegal_value;
        led_AKS = illegal_value;
        p_not_leading_S = illegal_value;
        points_played = 0;
        points_unplayed = 26;
        points_sit = unplayed;                          // unplayed; split; 0-3: player
        possible_QS_poison = illegal_value;
        QS_played = false;
        redeal = false;
        Shoot_try = illegal_value;
        suppress_msg_1 = false;
        suppress_msg_2 = false;
        suppress_msg_3 = false;
        trick = 0;

        trick_leader = get_next_player(dealer);         // trick_leader leads on T1
        if (twoC_leads)
        {
            trick_leader = illegal_value;
            for (var p = player_west; p <= player_south; p++)
                if (hands[p, 2, clubs] == 1)
                {
                    player_with_2C = p;
                    trick_leader = p;
                    break;
                }
            if (trick_leader == illegal_value)
                diagnose_and_halt("Cannot find 2C");
        }
        start_trick();
    }

    /// <summary>The start of play_hand's loop and of play_trick.</summary>
    void start_trick()
    {
        trick++;
        if (trick > 13)
            diagnose_and_halt("Number of points played after 13 tricks is not 26");
        T_left = 14 - trick;

        card_led = illegal_value;
        game_is_over = false;
        player = trick_leader;
        plays_AKS_in_trick = illegal_value;
        points_in_trick = 0;
        prev_W = illegal_value;
        QS_led_on_T = false;
        QS_on_trick = false;
        sluffs = 0;
        suit_led = illegal_value;
        trick_winner = illegal_value;
        W = illegal_value;
        for (var i = player_west; i <= player_south; i++)
        {
            trick_old[i] = trick_current[i];            // move_trick_current_to_trick_old
            trick_current[i] = illegal_value;
        }
        player_has_played.Clear();
        seq = 0;
    }

    /// <summary>
    /// The engine's card for the seat whose turn it is. As in the original, call this for every seat,
    /// even one whose card comes from outside, so the engine's state evolves as it would. If the engine
    /// claims the remaining tricks, Claimed is true and the result is the seat's highest legal card.
    /// </summary>
    public int ComputePlay()
    {
        Claimed = false;
        if (points_played == 26)
            return HighestLegalCard();                  // the original ends the hand once all points are played
        all_winners = false;
        this_is_best_player = play_skill[player] == best_player;
        show_thinking = false;
        determine_card_to_play_automatically();         // sets khrank_to_play, khsuit_to_play, can_lead_H
        Claimed = all_winners;
        if (all_winners)
        {
            // The original ends the hand here. Play continues in Hearts, so play on with any card:
            // all of the player's cards win.
            all_winners = false;
            all_winners_type = 0;
            return HighestLegalCard();
        }
        var card = 0;
        get_index_from_rank_and_suit(ref card, khrank_to_play, khsuit_to_play);
        return card;
    }

    int HighestLegalCard()
    {
        var best = illegal_value;
        var bestRank = 0;
        for (var s = spades; s <= diamonds; s++)
            for (var r = 2; r <= ace; r++)
            {
                if (hands[player, r, s] == 0 || !IsLegal(r, s))
                    continue;
                if (r > bestRank)
                {
                    bestRank = r;
                    get_index_from_rank_and_suit(ref best, r, s);
                }
            }
        return best;
    }

    /// <summary>Is the card legal for the current player, under the rules play_trick enforces?</summary>
    bool IsLegal(int r, int s)
    {
        if (hands[player, r, s] == 0)
            return false;
        var heldInSuit = 0;
        var heldNonPoint = 0;
        var heldNonHeart = 0;
        for (var ss = spades; ss <= diamonds; ss++)
            for (var rr = 2; rr <= ace; rr++)
            {
                if (hands[player, rr, ss] == 0)
                    continue;
                if (seq > 0 && ss == suit_led)
                    heldInSuit++;
                if (ss != hearts && !(ss == spades && rr == queen))
                    heldNonPoint++;
                if (ss != hearts)
                    heldNonHeart++;
            }
        if (seq == 0)
        {
            if (twoC_leads)
                return r == 2 && s == clubs;
            return s != hearts || can_lead_H || heldNonHeart == 0;
        }
        if (heldInSuit > 0)
            return s == suit_led;
        var isPoint = s == hearts || (s == spades && r == queen);
        return !isPoint || sluff_pts || heldNonPoint == 0;
    }

    /// <summary>Plays the card for the seat whose turn it is, as play_trick does.</summary>
    public void ApplyPlay(int card) => ApplyPlay(card, checkLegality: true);

    void ApplyPlay(int card, bool checkLegality)
    {
        get_rank_and_suit_from_index(card, ref khrank_to_play, ref khsuit_to_play);

        // Check legality of card selected to play
        if (!checkLegality)
        {
            // the seat's hand is not fully known (see Observer.cs)
            if (hands[player, khrank_to_play, khsuit_to_play] == 0)
                diagnose_and_halt("Card selected to play is not in the hand");
        }
        else if (khsuit_to_play < spades || khsuit_to_play > diamonds)
            diagnose_and_halt("Card selected to play is in an illegal suit");
        else if (hands[player, khrank_to_play, khsuit_to_play] == 0)
            diagnose_and_halt("Card selected to play is not in the hand");
        else if (seq == 0)
        {
            if (khsuit_to_play == hearts && !can_lead_H && !IsLegal(khrank_to_play, khsuit_to_play))
                diagnose_and_halt("Cannot lead hearts");
        }
        else if (khsuit_to_play != suit_led && CountInSuit(player, suit_led) > 0)
            diagnose_and_halt("Card selected to play is a reneg");

        // remove the selected card from hand, mark it as played, and add it to trick_current
        hands[player, khrank_to_play, khsuit_to_play] = 0;
        cards_played[khrank_to_play, khsuit_to_play] = 1;
        count_played[khsuit_to_play]++;
        player_has_played[player] = true;
        hand_size[player]--;
        trick_current[player] = card;
        if (khrank_to_play == pub_lowest_unplayed[khsuit_to_play])
        {
            pub_lowest_unplayed[khsuit_to_play] = 15;   // illegal card means all played
            for (var i = khrank_to_play; i <= ace; i++)
                if (cards_played[i, khsuit_to_play] == 0)
                {
                    pub_lowest_unplayed[khsuit_to_play] = i;
                    break;
                }
        }

        // analyze the trick so far
        analyze_trick_so_far();

        player = get_next_player(player);
        twoC_leads = false;
        seq++;
        if (seq == 4)
            end_trick();
    }

    int CountInSuit(int p, int s)
    {
        var n = 0;
        for (var r = 2; r <= ace; r++)
            n += hands[p, r, s];
        return n;
    }

    /// <summary>The end of play_trick, and of play_hand's loop.</summary>
    void end_trick()
    {
        score_in_hand[trick_winner] = score_in_hand[trick_winner] + points_in_trick;
        points_played = points_played + points_in_trick;
        points_unplayed = points_unplayed - points_in_trick;
        low_score = 1000;
        var score_2nd_low = 1000;
        var z = false;
        for (var i = player_west; i <= player_south; i++)
        {
            score_in_game[i] = score[i] + score_in_hand[i];
            if (score_in_game[i] >= final_score)
                z = true;
            if (score_in_game[i] <= low_score)
            {
                score_2nd_low = low_score;
                low_score = score_in_game[i];
            }
            else if (score_in_game[i] < score_2nd_low)
                score_2nd_low = score_in_game[i];
        }
        game_is_over = z & trick <= 12 & points_sit == split & points_unplayed > 0
            & low_score + points_unplayed < score_2nd_low;
        if (game_is_over)
            GameIsOver = true;
        trick_leader = trick_winner;
        tricks_won_by[trick_winner]++;

        sluff_pts = true;
        if (trick < 13)
            start_trick();
    }

    // ------------------------------------------------------------------------------------------------
    // Scoring

    /// <summary>
    /// Scores the hand, as the end of play_hand does, and answers each seat's game score after it. Call
    /// this once the hand is over: after the last trick, once all points are played, once GameIsOver, or
    /// when the current player claims the remaining tricks (and then before playing on).
    /// </summary>
    public int[] FinishHand(bool claimed)
    {
        if (claimed)
        {
            wins_all_remaining = player;
            score_in_hand[wins_all_remaining] = score_in_hand[wins_all_remaining] + points_unplayed;
            score_in_game[wins_all_remaining] = score_in_game[wins_all_remaining] + points_unplayed;
            if (points_sit == unplayed || points_sit == wins_all_remaining)
            {
                points_sit = wins_all_remaining;
                Shoot_try = wins_all_remaining;
            }
            else points_sit = split;
            points_played = 26;
            points_unplayed = 0;
        }

        if (points_sit == split)                        // points of hand are split
        {
            for (var p = player_west; p <= player_south; p++)
                score[p] = score[p] + score_in_hand[p];
        }
        else                                            // Moon Shoot by player (= points_sit)
        {
            set_score_between_hands();
            score_in_hand[points_sit] = 0;
            score_in_game[points_sit] = score_in_game[points_sit] - 26;
            if (score[points_sit] - 26 < score_between_hands[low])
            {
                for (var p = player_west; p <= player_south; p++) // add 26 points to others' score
                    if (p != points_sit)
                    {
                        score[p] = score[p] + 26;
                        score_in_hand[p] = 26;
                        score_in_game[p] = score_in_game[p] + 26;
                    }
            }
            else
            {
                score[points_sit] = score[points_sit] - 26;
                score_in_hand[points_sit] = -26;        // decrease Shooter's score by 26 points
                score_in_game[points_sit] = score_in_game[points_sit] - 26;
            }
        }
        set_score_between_hands();
        return [score[player_west], score[player_north], score[player_east], score[player_south]];
    }
}
