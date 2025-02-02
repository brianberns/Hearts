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
        /// Current player. [B, playerLength] of int.
        Player : Tensor

        /// Current player's hand. [B, handLength] of int.
        /// </summary>
        Hand : Tensor

        /// Unplayed cards not in the current player's hand. [B, otherUnplayedLength] of int.
        OtherUnplayed : Tensor

        /// Current unfinished trick. [B, trickLength] of int.
        Trick : Tensor

        /// Known void suits for each player. [B, voidsLength] of int.
        Voids : Tensor

        /// Each player's score. [B, scoreLength] of int.
        Score : Tensor
    }

module Encoding =

    /// Current player.
    let playerLength = 1

    /// Maximum number of cards in a hand.
    let handLength = ClosedDeal.numCardsPerHand

    /// Maximum number of unplayed cards not in the
    /// current player's hand.
    let otherUnplayedLength =
        Card.numCards - ClosedDeal.numCardsPerHand

    /// Maximum length of an unfinished trick.
    let trickLength = Seat.numSeats - 1

    /// Maximum number of known void suits.
    let voidsLength = Seat.numSeats * Suit.numSuits

    /// Each player's score.
    let scoreLength = Seat.numSeats

    let private length (tensor : Tensor) =
        Array.last tensor.shape |> int

    let create player hand otherUnplayed trick voids score =
        assert(
            [ player; hand; otherUnplayed; trick; voids; score ]
                |> Seq.map (fun (tensor : Tensor) ->
                    assert(tensor.shape.Length = 2)
                    tensor.shape[0])   // batch size
                |> Seq.distinct
                |> Seq.length = 1)
        assert(length player = playerLength)
        assert(length hand = handLength)
        assert(length otherUnplayed = otherUnplayedLength)
        assert(length trick = trickLength)
        assert(length voids = voidsLength)
        assert(length score = scoreLength)
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
        let ofRow (row : int[]) =
            tensor(row, device = torch.CPU).unsqueeze(dim = 0)   // batch size = 1

    /// Encodes the given player's seat.
    let private encodePlayer player =
        int player
            |> Array.singleton
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
            Array.replicate
                (maxNum - present.Length)
                Card.numCards
        Array.append present absent
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
            Array.replicate (maxNum - present.Length) maxNum
        Array.append present absent
            |> Tensor.ofRow

    /// Encodes the given score.
    let private encodeScore score =
        assert(score.ScoreMap.Count = Seat.numSeats)
        score.ScoreMap.Values
            |> Seq.toArray
            |> Tensor.ofRow

    /// Encodes the given info set (hand + deal).
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
