namespace Hearts.Model

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
type Encoding = byte[]

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

    /// Encodes the given unordered set of card as a single
    /// multi-hot vector in the deck size.
    let private encodeCardsSet (cards : Set<Card>) =
        cards
            |> Seq.map (fun card -> card, 1uy)
            |> encodeCardValues

    /// Encodes the given exchange directon as a one-hot
    /// vector in the number of seats.
    let private encodeExchangeDirection dir =
        [|
            for d in Enum.getValues<ExchangeDirection> do
                if d = dir then 1uy
                else 0uy
        |]

    /// Encodes the given pass as a multi-hot vector in
    /// the deck size. The cards in a pass are unordered.
    let private encodePass (passOpt : Option<Pass>) =
        let cards =
            passOpt
                |> Option.defaultValue Set.empty
        assert(cards.Count <= Pass.numCards)
        encodeCardsSet cards

    /// Maximum number of cards played at the last decision
    /// point of the game. This occurs when playing the last
    /// card on the penultimiate trick. (The last trick has
    /// no decision points, because each player has only one
    /// card left.) In a full game of Hearts, this is (12 * 4)
    /// - 1 = 47.
    let private maxNumCardsPlayed =
        ((ClosedDeal.numCardsPerHand - 1)   // number of interesting tricks
            * Seat.numSeats)                // number of cards per trick
            - 1                             // last card on the last interesting trick

    let private encodeSuit suitOpt =
        [|
            for st in Enum.getValues<Suit> do
                if Some st = suitOpt then 1uy
                else 0uy
        |]

    let private encodeRank rankOpt =
        [|
            for rk in Enum.getValues<Rank> do
                if Some rk = rankOpt then 1uy
                else 0uy
        |]

    let private encodeCardOpt (cardOpt : Option<Card>) =
        [|
            yield! cardOpt
                |> Option.map _.Suit
                |> encodeSuit
            yield! cardOpt
                |> Option.map _.Rank
                |> encodeRank
        |]

    /// Encodes all cards played so far.
    let private encodeCardsPlayed deal =
        let cards =
            ClosedDeal.tricks deal
                |> Seq.collect (
                    Trick.plays >> Seq.map snd)
                |> Seq.toArray
        [|
            for iCard = 0 to maxNumCardsPlayed - 1 do
                yield!
                    if iCard < cards.Length then
                        Some cards[iCard]
                    else None
                    |> encodeCardOpt
        |]

    /// Total encoded length of an info set.
    let encodedLength =
        Card.numCards                               // current player's hand
            + ExchangeDirection.numDirections       // exchange direction
            + Card.numCards                         // outgoing pass
            + Card.numCards                         // incoming pass
            + (maxNumCardsPlayed                    // cards played
                * (Suit.numSuits + Rank.numRanks))

    /// Encodes the given info set as a vector.
    let encode infoSet : Encoding =
        let encoded =
            [|
                yield! encodeCardsSet infoSet.Hand          // current player's hand (unordered)
                yield! encodeExchangeDirection              // exchange direction
                    infoSet.Deal.ExchangeDirection
                yield! encodePass infoSet.OutgoingPassOpt   // outgoing pass
                yield! encodePass infoSet.IncomingPassOpt   // incoming pass
                yield! encodeCardsPlayed infoSet.Deal       // cards played
            |]
        assert(encoded.Length = encodedLength)
        encoded
