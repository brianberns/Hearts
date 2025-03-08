namespace Hearts

open PlayingCards
open Hearts

/// Information known only to a player about her own hand.
type Secret =
    {
        /// Player's hand.
        Hand : Hand

        /// Cards passed by the player, if any.
        OutgoingPassOpt : Option<Pass>

        /// Cards received by the player, if any.
        IncomingPassOpt : Option<Pass>
    }

module Secret =

    /// Creates a secret.
    let create hand outgoingPassOpt incomingPassOpt =
        assert(Option.isNone incomingPassOpt
            || Option.isSome outgoingPassOpt)
        {
            Hand = hand
            OutgoingPassOpt = outgoingPassOpt
            IncomingPassOpt = incomingPassOpt
        }

/// All information known to a player about a deal.
type InformationSet =
    {
        /// Player.
        Player : Seat

        /// Player's secret information.
        Secret : Secret

        /// Public information.
        Deal : ClosedDeal
    }

module InformationSet =

    let create player secret deal =
        assert(deal.CurrentTrickOpt.IsNone
            || ClosedDeal.currentPlayer deal = player)
        assert(ClosedDeal.isComplete deal |> not)
        {
            Player = player
            Secret = secret
            Deal = deal
        }
