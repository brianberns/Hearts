namespace KHearts;

/// <summary>
/// Sets up a table from one seat's point of view, for playing that seat alone. The seat's own hand and
/// passes are exact. The other seats' unplayed cards are unknown, so they are filled in arbitrarily:
/// the engine never reads another seat's hand, so these fillers affect nothing but bookkeeping.
///
/// What a single seat cannot reproduce is the side effects of the engine deciding for the other seats,
/// which the original computes from their real hands. The one with a known effect on play is the
/// global mode, which a public inference reads right after each card; StandInMode supplies it. Against
/// an exact four-seat table of Killer Hearts players, the one-seat player makes the same decision 99.98%
/// of the time.
/// </summary>
public partial class KhEngine
{
    /// <summary>
    /// Starts a deal from the observer's point of view, at the observer's turn to pass. The other seats'
    /// hands are any cards not in the observer's.
    /// </summary>
    public void StartObservedPassing(int observer, int dealerSeat, int passDirection, int[] gameScore,
        int[] dealtHand, bool isFirstDeal)
    {
        var hands = FillOthers(observer, dealtHand, _ => []);
        StartDeal(dealerSeat, passDirection, gameScore, hands, isFirstDeal);
        if (passDirection == hold)
            throw new InvalidOperationException("No passing on a hold hand");
        while (passer != observer)
        {
            passes_made++;
            NextPasser();
        }
    }

    /// <summary>
    /// Starts a deal from the observer's point of view, ready to replay play from the first trick. The
    /// observer's dealt hand and passes are exact; each other seat holds at least the cards it has played.
    /// </summary>
    public void StartObservedPlay(int observer, int dealerSeat, int passDirection, int[] gameScore,
        int[] dealtHand, int[] passedCards, int[] receivedCards, Func<int, int[]> playedBy, bool isFirstDeal)
    {
        dealer = dealerSeat;
        pass_direction = passDirection;
        first_hand = isFirstDeal;
        deal_number = 0;
        for (var p = player_west; p <= player_south; p++)
        {
            if (gameScore[p] < 0 || gameScore[p] > 125)
                throw new ArgumentOutOfRangeException(nameof(gameScore), "Scores must be 0 to 125");
            score[p] = gameScore[p];
            score_in_game[p] = gameScore[p];
        }
        set_score_between_hands();
        twoC_leads = true;
        sluff_pts = false;

        // the observer's hand after the pass
        var held = new HashSet<int>(dealtHand);
        held.ExceptWith(passedCards);
        held.UnionWith(receivedCards);
        var receiver = passDirection == hold ? illegal_value : (observer + player_incr[passDirection]) % 4;

        // the others' hands after the pass: the cards they play, the observer's pass for its receiver, and fillers
        var postPass = FillOthers(observer, [.. held], p =>
            p == receiver ? [.. playedBy(p).Union(passedCards)] : playedBy(p));

        private_info.Clear();
        hands.Clear();
        for (var p = player_west; p <= player_south; p++)
            foreach (var card in postPass[p])
            {
                var r = 0;
                var s = 0;
                get_rank_and_suit_from_index(card, ref r, ref s);
                hands[p, r, s] = 1;
            }

        // what the observer knew at the deal: no opponent has any of its dealt cards
        foreach (var card in dealtHand)
        {
            var r = 0;
            var s = 0;
            get_rank_and_suit_from_index(card, ref r, ref s);
            for (var k = 1; k <= 3; k++)
                private_info[observer, (observer + k) % 4, r, s] = 0x20F;
        }
        for (var p = player_west; p <= player_south; p++)
            hand_size[p] = 13;

        for (var i = player_west; i <= player_south; i++)
        {
            passed_AS[i] = illegal_value;
            passed_KS[i] = illegal_value;
            passed_QS[i] = illegal_value;
        }
        if (passDirection != hold)
        {
            for (var i = player_west; i <= player_south; i++)
            {
                p_p[i] = (i - player_incr[pass_direction] + 4) % 4;
                r_p[i] = (i + player_incr[pass_direction]) % 4;
                for (var k = 1; k <= 3; k++)
                {
                    recd_p_rank[i, k] = 2;              // other seats' passes are unknown: these stand-ins
                    recd_p_suit[i, k] = spades;         // only affect the other seats' private knowledge
                }
            }
            SetPass(observer, receivedCards);           // what the observer received
            SetPass(receiver, passedCards);             // what the observer passed
            foreach (var card in passedCards)
            {
                var r = 0;
                var s = 0;
                get_rank_and_suit_from_index(card, ref r, ref s);
                if (s != spades)
                    continue;
                if (r == ace) passed_AS[observer] = receiver;
                else if (r == king) passed_KS[observer] = receiver;
                else if (r == queen) passed_QS[observer] = receiver;
            }
            update_private_info_with_pass_information();
        }
        start_play();
    }

    void SetPass(int recipient, int[] cards)
    {
        for (var k = 1; k <= 3; k++)
            get_rank_and_suit_from_index(cards[k - 1], ref recd_p_rank[recipient, k], ref recd_p_suit[recipient, k]);
    }

    /// <summary>
    /// Gives each other seat 13 cards: those it must hold, then fillers from the cards no one must hold.
    /// </summary>
    static int[][] FillOthers(int observer, int[] observerHand, Func<int, int[]> mustHold)
    {
        var hands = new int[4][];
        var taken = new HashSet<int>(observerHand);
        for (var p = 0; p < 4; p++)
            if (p != observer)
            {
                hands[p] = mustHold(p);
                taken.UnionWith(hands[p]);
            }
        var fillers = new Queue<int>(Enumerable.Range(0, 52).Where(c => !taken.Contains(c)));
        hands[observer] = observerHand;
        for (var p = 0; p < 4; p++)
            if (p != observer)
            {
                var hand = new List<int>(hands[p]);
                while (hand.Count < 13)
                    hand.Add(fillers.Dequeue());
                if (hand.Count != 13)
                    throw new InvalidOperationException($"Seat {p} must hold {hand.Count} cards");
                hands[p] = [.. hand];
            }
        return hands;
    }

    /// <summary>
    /// Plays a card for a seat whose decision the engine cannot compute, because only the observer's hand
    /// is known. The original would have computed that seat's decision first, from its real hand; this
    /// supplies the visible effects of that: the seat's mode, and the trick-1 permission to sluff points
    /// that a hand of nothing but point cards grants.
    /// </summary>
    public void ApplyObservedPlay(int card)
    {
        var r = 0;
        var s = 0;
        get_rank_and_suit_from_index(card, ref r, ref s);
        mode = StandInMode;
        if (trick == 1 && seq > 0 && s != suit_led && (s == hearts || (s == spades && r == queen)))
            sluff_pts = true;                           // only a hand of nothing but point cards can do this
        ApplyPlay(card, checkLegality: false);
    }

    /// <summary>
    /// A stand-in for another seat's mode. Only whether it is Stop_Shoot matters (update_void_records_and_
    /// other_parameters reads it once), and assuming it never is matches the exact four-seat table best:
    /// in 202,748 decisions, the one-seat player differs 50 times, against 383 when assuming Stop_Shoot
    /// whenever another seat holds every point, and 87 when leaving the mode as the observer's own.
    /// </summary>
    const int StandInMode = Lie_Low;
}
