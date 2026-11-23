using Hearts;
using Microsoft.FSharp.Core;
using PlayingCards;
using Card = PlayingCards.Card;

namespace KHearts;

/// <summary>
/// Killer Hearts, Gerald Berns' Hearts program, as a Hearts player. It plays at the original's best
/// skill level, assuming its opponents do too.
/// </summary>
public static class KillerHearts
{
    /// <summary>
    /// Creates a player that sees the game score given by the function before each deal. Killer Hearts
    /// plays differently depending on the game score.
    /// </summary>
    public static Player CreatePlayer(Func<InformationSet, Score> getGameScore) =>
        new(FuncConvert.FromFunc<InformationSet, Card>(infoSet =>
            Act(infoSet, getGameScore(infoSet).Points)));

    /// <summary>
    /// Creates a player that sees the information set's game score, as the arena bot does (see EngineScores).
    /// </summary>
    public static Player CreatePlayer() =>
        CreatePlayer(infoSet => new Score(EngineScores(infoSet.GameScore.Points)));

    /// <summary>
    /// Scores as Killer Hearts expects them: none 100 or more before a deal. A game that continues after a
    /// tie at 100 or more (Rules.arena) would otherwise overflow the engine's score arrays, so scores are
    /// shifted down until the highest is 99. The same as engine_scores in the arena bot.
    /// </summary>
    public static int[] EngineScores(int[] scores)
    {
        var shift = Math.Max(scores.Max() - 99, 0);
        return [.. scores.Select(s => Math.Clamp(s - shift, 0, 99))];
    }

    /// <summary>Creates a player that ignores the game score, treating every deal as the first deal of a game.</summary>
    public static Player CreateScoreBlindPlayer() =>
        CreatePlayer(_ => new Score(new int[4]));

    /// <summary>Chooses an action in the given information set, given the game score before the deal.</summary>
    public static Card Act(InformationSet infoSet, int[] gameScore)
    {
        if (infoSet.LegalActions.Length == 1)
            return infoSet.LegalActions[0];
        var card = Decide(infoSet, gameScore);
        if (!infoSet.LegalActions.Contains(card))
            throw new InvalidOperationException($"Killer Hearts chose an illegal card: {card}");
        return card;
    }

    static Card Decide(InformationSet infoSet, int[] gameScore)
    {
        var me = (int)infoSet.Player;
        var deal = infoSet.Deal;
        var dealer = (int)deal.Dealer;
        var direction = (int)deal.ExchangeDirection;    // the same numbering in both
        var isFirstDeal = gameScore.All(s => s == 0);
        var engine = new KhEngine();

        var hand = infoSet.Hand.Select(ToKh).ToArray();
        var passed = Cards(infoSet.OutgoingPassOpt);

        if (infoSet.LegalActionType.IsPass)
        {
            // the engine chooses all three cards at once, from the dealt hand
            int[] dealt = [.. hand, .. passed];
            engine.StartObservedPassing(me, dealer, direction, gameScore, dealt, isFirstDeal);
            var pass = engine.ComputePass();
            var next = pass.First(c => !passed.Contains(c));
            return FromKh(next);
        }

        return FromKh(ReplayPlay(infoSet, gameScore).ComputePlay());
    }

    /// <summary>The engine's main choices for a pass, for checking the port (see KhEngine.BriefPass).</summary>
    public static string TracePass(InformationSet infoSet, int[] gameScore)
    {
        var me = (int)infoSet.Player;
        var passed = Cards(infoSet.OutgoingPassOpt);
        int[] dealt = [.. infoSet.Hand.Select(ToKh), .. passed];
        var engine = new KhEngine();
        engine.StartObservedPassing(me, (int)infoSet.Deal.Dealer, (int)infoSet.Deal.ExchangeDirection, gameScore, dealt,
            gameScore.All(s => s == 0));
        var pass = engine.ComputePass();
        return $"{engine.BriefPass()} pass={string.Join(" ", pass.Select(FromKh))}";
    }

    /// <summary>The engine's main choices for a play decision, for checking the port (see KhEngine.Brief).</summary>
    public static string TracePlay(InformationSet infoSet, int[] gameScore)
    {
        var engine = ReplayPlay(infoSet, gameScore);
        engine.ComputePlay();
        return engine.Brief();
    }

    /// <summary>An engine that has replayed the deal so far, with the information set's player to play.</summary>
    static KhEngine ReplayPlay(InformationSet infoSet, int[] gameScore)
    {
        var me = (int)infoSet.Player;
        var deal = infoSet.Deal;
        var dealer = (int)deal.Dealer;
        var direction = (int)deal.ExchangeDirection;
        var isFirstDeal = gameScore.All(s => s == 0);
        var engine = new KhEngine();
        var hand = infoSet.Hand.Select(ToKh).ToArray();
        var passed = Cards(infoSet.OutgoingPassOpt);
        var received = Cards(infoSet.IncomingPassOpt);

        // replay the deal so far, letting the engine decide each of the observer's earlier turns too
        var plays = Plays(deal).ToArray();
        var played = Enumerable.Range(0, 4)
            .Select(p => plays.Where(x => x.Seat == p).Select(x => x.Card).ToArray())
            .ToArray();
        int[] dealtHand = [.. hand.Concat(played[me]).Concat(passed).Except(received)];
        engine.StartObservedPlay(me, dealer, direction, gameScore, dealtHand, passed, received,
            p => played[p], isFirstDeal);
        foreach (var (seat, card) in plays)
        {
            if (engine.CurrentPlayer != seat)
                throw new InvalidOperationException($"Replay is out of step: {seat} played, {engine.CurrentPlayer} expected");
            if (seat == me)
            {
                engine.ComputePlay();
                engine.ApplyPlay(card);
            }
            else
                engine.ApplyObservedPlay(card);
            if (card == QueenOfSpades && infoSet.Rules.HeartsBreak.IsHeartsOnly)
                engine.UnbreakHearts();
        }
        if (engine.CurrentPlayer != me)
            throw new InvalidOperationException($"Replay ends with {engine.CurrentPlayer} to play, not {me}");
        return engine;
    }

    /// <summary>The Q♠, as the engine numbers cards.</summary>
    static readonly int QueenOfSpades = ToKh(new Card(Rank.Queen, Suit.Spades));

    /// <summary>Every card played in the deal so far, in order, with its player.</summary>
    static IEnumerable<(int Seat, int Card)> Plays(ClosedDeal deal)
    {
        var tricks = deal.CompletedTricks.Reverse().ToList();
        if (FSharpOption<Trick>.get_IsSome(deal.CurrentTrickOpt))
            tricks.Add(deal.CurrentTrickOpt.Value);
        foreach (var trick in tricks)
        {
            var seat = (int)trick.Leader;
            foreach (var card in trick.Cards.Reverse())
            {
                yield return (seat, ToKh(card));
                seat = (seat + 1) % 4;
            }
        }
    }

    static int[] Cards(FSharpOption<Microsoft.FSharp.Collections.FSharpSet<Card>> passOpt) =>
        FSharpOption<Microsoft.FSharp.Collections.FSharpSet<Card>>.get_IsSome(passOpt)
            ? [.. passOpt.Value.Select(ToKh)]
            : [];

    /// <summary>Killer Hearts numbers suits spades, hearts, clubs, diamonds.</summary>
    static int KhSuit(Suit suit) =>
        suit switch
        {
            Suit.Spades => 0,
            Suit.Hearts => 1,
            Suit.Clubs => 2,
            Suit.Diamonds => 3,
            _ => throw new ArgumentException($"Unexpected suit {suit}"),
        };

    static readonly Suit[] suits = [Suit.Spades, Suit.Hearts, Suit.Clubs, Suit.Diamonds];

    /// <summary>Converts a card to a Killer Hearts card number.</summary>
    public static int ToKh(Card card) => 4 * ((int)card.Rank - 2) + KhSuit(card.Suit);

    /// <summary>Converts a Killer Hearts card number to a card.</summary>
    public static Card FromKh(int card) => new((Rank)(card / 4 + 2), suits[card % 4]);
}
