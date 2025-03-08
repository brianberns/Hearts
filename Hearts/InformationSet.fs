namespace Hearts

open PlayingCards
open Hearts

/// An action is either a pass (during the exchange) or
/// a play (after the exchange).
[<RequireQualifiedAccess>]
type ActionType = Pass | Play

/// Cards passed from one player to another.
type Pass = Set<Card>

module Pass =

    /// Number of cards passed by each player.
    let numCards = 3

    /// Empty pass to which cards will be added.
    let empty : Pass = Set.empty

    /// Is the given pass ready to be delivered?
    let isComplete (pass : Pass) =
        assert(pass.Count <= numCards)
        pass.Count = numCards

    /// Adds the given card to the given pass.
    let add card (pass : Pass) : Pass =
        assert(pass.Count < numCards)
        assert(pass.Contains(card) |> not)
        pass.Add(card)

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
                ActionType.Pass
            | _ -> ActionType.Play

    /// What actions can be taken in the given information set?
    let legalActions infoSet =
        let actionType = legalActionType infoSet
        let actions =
            match actionType with
                | ActionType.Pass ->
                    assert(
                        infoSet.Deal.ExchangeDirection
                            <> ExchangeDirection.Hold)
                    Seq.toArray infoSet.Hand   // pass any card in hand
                | ActionType.Play ->
                    assert(
                        infoSet.Deal.ExchangeDirection
                            = ExchangeDirection.Hold
                        || (infoSet.OutgoingPassOpt.Value.Count
                                = Pass.numCards
                            && infoSet.IncomingPassOpt.Value.Count
                                = Pass.numCards))
                    let legalPlays =
                        ClosedDeal.legalPlays
                            infoSet.Hand infoSet.Deal
                    Seq.toArray legalPlays
        actionType, actions
