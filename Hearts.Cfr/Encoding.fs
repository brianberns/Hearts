namespace Hearts.Cfr

open System
open System.Collections

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

/// Encoded value for input to a model.
type Encoding = BitArray

module Encoding =

    /// Encodes the given (card, value) pairs as a
    /// vector in the deck size.
    let inline encodeCardValues pairs =
        let valueMap =
            pairs
                |> Seq.map (fun (card, value) ->
                    Card.toIndex card, value)
                |> Map
        [|
            for index = 0 to Card.numCards - 1 do
                valueMap
                    |> Map.tryFind index
                    |> Option.defaultValue
                        LanguagePrimitives.GenericZero   // encode to input type
        |]

    /// Encodes the given cards as a multi-hot vector
    /// in the deck size.
    let private encodeCards cards =
        let cardIndexes =
            cards
                |> Seq.map Card.toIndex
                |> set
        [|
            for index = 0 to Card.numCards - 1 do
                cardIndexes.Contains(index)
        |]

    /// Encodes each card in the given current trick as
    /// a one-hot vector in the deck size and concatenates
    /// those vectors.
    let private encodeTrick trickOpt =
        let cards =
            trickOpt
                |> Option.map (fun trick ->
                    trick.Cards
                        |> List.rev
                        |> List.toArray)
                |> Option.defaultValue Array.empty
        assert(cards.Length < Seat.numSeats)
        [|
            for iCard = 0 to Seat.numSeats - 2 do
                yield!
                    if iCard < cards.Length then
                        Some cards[iCard]
                    else None
                    |> Option.toArray
                    |> encodeCards
        |]

    /// Encodes the given voids as a multi-hot vector in the
    /// number of suits times the number of other seats.
    let private encodeVoids player voids =
        let seats =
            Seat.cycle player |> Seq.skip 1
        [|
            for suit in Enum.getValues<Suit> do
                for seat in seats do
                    Set.contains (seat, suit) voids
        |]

    /// Encodes the given score as a multi-hot vector in the
    /// number of seats.
    let private encodeScore player score =
        assert(score.ScoreMap.Count = Seat.numSeats)
        [|
            for seat in Seat.cycle player do
                score.ScoreMap[seat] > 0
        |]

    /// Total encoded length of an info set.
    let encodedLength =
        Card.numCards                                 // current player's hand
            + Card.numCards                           // unplayed cards not in current player's hand
            + ((Seat.numSeats - 1) * Card.numCards)   // current trick
            + ((Seat.numSeats - 1) * Suit.numSuits)   // voids
            + Seat.numSeats                           // score

    /// Encodes the given info set as a vector.
    let encode infoSet : Encoding =
        let unseen =
            infoSet.Deal.UnplayedCards - infoSet.Hand
        let trickOpt = infoSet.Deal.CurrentTrickOpt
        let encoded =
            BitArray [|
                yield! encodeCards infoSet.Hand         // current player's hand
                yield! encodeCards unseen               // unplayed cards not in current player's hand
                yield! encodeTrick trickOpt             // current trick
                yield! encodeVoids                      // voids
                    infoSet.Player infoSet.Deal.Voids
                yield! encodeScore                      // score
                    infoSet.Player infoSet.Deal.Score
            |]
        assert(encoded.Length = encodedLength)
        encoded

    /// "Latin Extended-A" block is printable.
    let private charOffset = 0x100

    /// Converts a byte array to a compact, printable Unicode string.
    let private compact bytes =
        bytes
            |> Array.map (fun (b : byte) ->
                char (int b + charOffset))
            |> String

    let toString (encoding : Encoding) =
        assert(encoding.Length = encodedLength)
        let bytes =
            let nBytes = (encoding.Length + 7) >>> 3
            Array.zeroCreate<byte> nBytes
        encoding.CopyTo(bytes, 0)
        compact bytes
