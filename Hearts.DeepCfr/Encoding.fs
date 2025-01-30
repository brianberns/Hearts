namespace Hearts.DeepCfr

open TorchSharp
open type torch
open FSharp.Core.Operators   // reclaim "float32" and other F# operators

open PlayingCards
open Hearts

module Card =

    /// Rank of lowest card in the deck.
    let private minRank =
        Seq.min Enum.getValues<Rank>

    /// Converts the given card to an integer, 0..N-1,
    /// where N is number of cards in the deck.
    let toIndex (card : Card) =
        let index =
            (int card.Suit * Rank.numRanks)
                + int card.Rank - int minRank
        assert(index >= 0)
        assert(index < Card.numCards)
        index

type Encoding =
    {
        /// Current player. [B, 1] of int.
        Player : Tensor

        /// Current player's hand. [B, ClosedDeal.numCardsPerHand] of int.
        Hand : Tensor

        /// Other unplayed cards. [B, Card.numCards - ClosedDeal.numCardsPerHand] of int.
        OtherUnplayed : Tensor

        /// Current trick. [B, Seat.numSeats - 1] of int.
        Trick : Tensor

        /// Known void suits for each other player. [B, Seat.numSeats * Suit.numSuits] of int.
        Voids : Tensor

        /// Each player's score. [B, Seat.numSeats] of int.
        Score : Tensor
    }

module Encoding =

    let private length (tensor : Tensor) =
        Array.last tensor.shape |> int

    let create player hand otherUnplayed trick voids score =
        assert(length player = 1)
        assert(length hand = ClosedDeal.numCardsPerHand)
        assert(length otherUnplayed = Card.numCards - ClosedDeal.numCardsPerHand)
        assert(length trick = Seat.numSeats - 1)
        assert(length voids = Seat.numSeats * Suit.numSuits)
        assert(length score = Seat.numSeats)
        {
            Player = player
            Hand = hand
            OtherUnplayed = otherUnplayed
            Trick = trick
            Voids = voids
            Score = score
        }

    let concat encodings =
        let catMap mapping =
            encodings
                |> Array.map mapping
                |> torch.cat
        create
            (catMap _.Player)
            (catMap _.Hand)
            (catMap _.OtherUnplayed)
            (catMap _.Trick)
            (catMap _.Voids)
            (catMap _.Score)

    module private Tensor =

        /// Creates a [1, N] tensor from a row of N values.
        let ofRow (row : seq<int>) =
            tensor(
                Seq.toArray row,
                device = settings.Device)
                .unsqueeze(dim = 0)   // batch size = 1

    /// Encodes the given player's seat.
    let private encodePlayer player =
        int player
            |> Seq.singleton
            |> Tensor.ofRow

    /// Encodes the given sequence of cards.
    let private encodeCards maxNum cards =
        let present =
            [|
                for card in cards do
                    yield Card.toIndex card
            |]
        assert(present.Length <= maxNum)
        let absent =
            Seq.replicate
                (maxNum - present.Length)
                Card.numCards
        Seq.append present absent
            |> Tensor.ofRow

    let private encodeHand (hand : Hand) =
        encodeCards ClosedDeal.numCardsPerHand hand

    let private encodeOtherUnplayed hand unplayedCards =
        encodeCards
            (Card.numCards - ClosedDeal.numCardsPerHand)
            (Set.difference unplayedCards hand)

    let private encodeTrick trick =
        encodeCards
            (Seat.numSeats - 1)
            (Seq.rev trick.Cards)

    /// Encodes the given voids.
    let private encodeVoids voids =
        let maxNum = Seat.numSeats * Suit.numSuits
        let present =
            [|
                for seat in Enum.getValues<Seat> do
                    for suit in Enum.getValues<Suit> do
                        if Set.contains (seat, suit) voids then
                            let index =
                                (Suit.numSuits * int seat) + int suit
                            assert(index < maxNum)
                            index
            |]
        assert(present.Length < maxNum)
        let absent =
            Seq.replicate (maxNum - present.Length) maxNum
        Seq.append present absent
            |> Tensor.ofRow

    /// Encodes the given score.
    let private encodeScore score =
        assert(score.ScoreMap.Count = Seat.numSeats)
        score.ScoreMap.Values
            |> Tensor.ofRow

    /// Encodes the given info set as a vector.
    let encode (hand : Hand) deal =
        let trick = ClosedDeal.currentTrick deal
        let player = Trick.currentPlayer trick
        create
            (encodePlayer player)
            (encodeHand hand)
            (encodeOtherUnplayed hand deal.UnplayedCards)
            (encodeTrick trick)
            (encodeVoids deal.Voids)
            (encodeScore deal.Score)
