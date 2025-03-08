namespace Hearts

open PlayingCards
open Hearts

/// An action is either a pass (during the exchange) or
/// a play (after the exchange).
type ActionType = Pass | Play

/// All information known to a player about a deal,
/// including information known only by that player.
type InformationSet =
    {
        /// Player.
        Player : Seat

        /// Player's hand.
        Hand : Hand

        /// Cards passed by the player, if any.
        OutgoingPassOpt : Option<Pass>

        /// Cards received by the player, if any.
        IncomingPassOpt : Option<Pass>

        /// Public information.
        Deal : ClosedDeal
    }

module InformationSet =

    /// Creates an information set.
    let create player hand outgoingPassOpt incomingPassOpt deal =
        assert(Option.isNone incomingPassOpt
            || Option.isSome outgoingPassOpt)
        assert(deal.CurrentTrickOpt.IsNone
            || ClosedDeal.currentPlayer deal = player)
        assert(ClosedDeal.isComplete deal |> not)
        {
            Player = player
            Hand = hand
            OutgoingPassOpt = outgoingPassOpt
            IncomingPassOpt = incomingPassOpt
            Deal = deal
        }

    /// What actions can be taken in the given information set?
    let legalActions infoSet =
        match infoSet.OutgoingPassOpt with
            | Some pass ->
                assert(
                    infoSet.Deal.ExchangeDirection
                        <> ExchangeDirection.Hold)
                Pass, Seq.toArray infoSet.Hand   // pass any card in hand
            | None ->
                assert(
                    infoSet.Deal.ExchangeDirection
                        = ExchangeDirection.Hold
                    || (infoSet.OutgoingPassOpt.Value.Count = Pass.numCards
                        && infoSet.IncomingPassOpt.Value.Count = Pass.numCards))
                let legalPlays =
                    ClosedDeal.legalPlays
                        infoSet.Hand infoSet.Deal
                Play, Seq.toArray legalPlays
