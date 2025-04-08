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

        /// What action type can be taken in this information set?
        LegalActionType : ActionType

        /// What actions can be taken in this information set?
        LegalActions : Card[]
    }

module InformationSet =

    /// What action type can be taken?
    let private legalActionType
        (outgoingPassOpt : Option<Pass>)
        (incomingPassOpt : Option<Pass>)
        deal =
        match outgoingPassOpt with
            | Some pass when pass.Count < Pass.numCards ->
                assert(
                    deal.ExchangeDirection
                        <> ExchangeDirection.Hold)
                ActionType.Pass
            | _ ->
                assert(
                    deal.ExchangeDirection
                        = ExchangeDirection.Hold
                    || (outgoingPassOpt.Value.Count
                            = Pass.numCards
                        && incomingPassOpt.Value.Count
                            = Pass.numCards))
                ActionType.Play

    /// What actions can be taken?
    let private legalActions actionType (hand : Hand) deal =
        match actionType with
            | ActionType.Pass ->
                Seq.toArray hand   // pass any card in hand
            | ActionType.Play ->
                ClosedDeal.legalPlays hand deal
                    |> Seq.toArray

    /// Creates an information set.
    let create player hand outgoingPassOpt incomingPassOpt deal =
        assert(Option.isNone incomingPassOpt
            || Option.isSome outgoingPassOpt)
        assert(deal.CurrentTrickOpt.IsNone
            || ClosedDeal.currentPlayer deal = player)
        assert(ClosedDeal.isComplete deal |> not)
        let actionType =
            legalActionType outgoingPassOpt incomingPassOpt deal
        {
            Player = player
            Hand = hand
            OutgoingPassOpt = outgoingPassOpt
            IncomingPassOpt = incomingPassOpt
            Deal = deal
            LegalActionType = actionType
            LegalActions = legalActions actionType hand deal
        }

/// Interface for a Hearts player.
type Player =
    {
        /// Chooses an action in the given information set.
        Act : InformationSet -> Card
    }
