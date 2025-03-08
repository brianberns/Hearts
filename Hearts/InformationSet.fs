namespace Hearts

open PlayingCards
open Hearts

type Secret =
    {
        Hand : Hand
        OutgoingPassOpt : Option<Pass>
        IncomingPassOpt : Option<Pass>
    }

module Secret =

    let create hand outgoingPassOpt incomingPassOpt =
        assert(Option.isNone incomingPassOpt
            || Option.isSome outgoingPassOpt)
        {
            Hand = hand
            OutgoingPassOpt = outgoingPassOpt
            IncomingPassOpt = incomingPassOpt
        }

type InformationSet =
    {
        Player : Seat
        Secret : Secret
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
