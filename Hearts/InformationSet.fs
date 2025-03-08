namespace Hearts

open PlayingCards
open Hearts

/// An action is either a pass (during the exchange) or
/// a play (after the exchange).
[<RequireQualifiedAccess>]
type ActionType = Pass | Play

/// All information known to a player about a deal,
/// including information known only to that player.
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

    /// What action type can be taken in the given information set?
    let legalActionType infoSet =
        match infoSet.OutgoingPassOpt with
            | Some pass when pass.Count < Pass.numCards ->
                assert(
                    infoSet.Deal.ExchangeDirection
                        <> ExchangeDirection.Hold)
                ActionType.Pass
            | _ ->
                assert(
                    infoSet.Deal.ExchangeDirection
                        = ExchangeDirection.Hold
                    || (infoSet.OutgoingPassOpt.Value.Count
                            = Pass.numCards
                        && infoSet.IncomingPassOpt.Value.Count
                            = Pass.numCards))
                ActionType.Play

    /// What actions can be taken in the given information set?
    let legalActions infoSet =
        let actionType = legalActionType infoSet
        let actions =
            match actionType with
                | ActionType.Pass ->
                    Seq.toArray infoSet.Hand   // pass any card in hand
                | ActionType.Play ->
                    ClosedDeal.legalPlays
                        infoSet.Hand infoSet.Deal
                        |> Seq.toArray
        actionType, actions

/// Interface for a Hearts player.
type Player =
    {
        /// Chooses an action in the given information set.
        Act : InformationSet -> ActionType * Card
    }
