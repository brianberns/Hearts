namespace Hearts.Cfr

open System

open MathNet.Numerics.LinearAlgebra

open PlayingCards
open Hearts
open Hearts.Model

module Encoding =

    /// Encodes cards played by in the given tricks as a
    /// multi-hot vector for each player.
    let encodePlays player tricks =
        let seatPlayMap =
            tricks
                |> Seq.collect Trick.plays
                |> Seq.toArray
                |> Array.groupBy fst
                |> Array.map (fun (seat, group) ->
                    let cards = Array.map snd group
                    seat, cards)
                |> Map
        [|
            for seat in Seat.cycle player do
                yield!
                    Map.tryFind seat seatPlayMap
                        |> Option.defaultValue Array.empty
                        |> Encoding.encodeCards
        |]

    /// Encodes each card in the given current trick as
    /// a one-hot vector in the deck size and concatenates
    /// those vectors.
    let encodeTrick trickOpt =
        let cards =
            trickOpt
                |> Option.map (_.Cards >> Seq.toArray)
                |> Option.defaultValue Array.empty
        assert(cards.Length < Seat.numSeats)
        [|
            for iCard = 0 to Seat.numSeats - 2 do
                yield!
                    if iCard < cards.Length then
                        Some cards[cards.Length - 1 - iCard]   // unreverse into chronological order
                    else None
                    |> Option.toArray
                    |> Encoding.encodeCards
        |]

    /// Encodes the given voids as a multi-hot vector in the
    /// number of suits times the number of other seats.
    let encodeVoids player voids =
        let flags =
            Array.zeroCreate ((Seat.numSeats - 1) * Suit.numSuits)
        for seat, suit in voids do
            if seat <> player then
                let suitOffset = (Seat.numSeats - 1) * int suit
                let seatOffset =
                    ((int seat - int player - 1) + Seat.numSeats)
                        % Seat.numSeats
                flags[suitOffset + seatOffset] <- true   // use mutation for speed
        flags

    /// Encodes the given score as a "thermometer" for each player.
    let encodeScore player score =
        assert(score.Points.Length = Seat.numSeats)
        [|
            for seat in Seat.cycle player do
                yield! Array.init
                    ClosedDeal.numPointsPerDeal
                    (fun i -> i < score[seat])
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
        let flags =
            [|
                yield! Encoding.encodeCards infoSet.Hand   // current player's hand
                yield! Encoding.encodeCards unseen         // unplayed cards not in current player's hand
                yield! encodeTrick trickOpt                // current trick
                yield! encodeVoids                         // voids
                    infoSet.Player infoSet.Deal.Voids
                yield! encodeScore                         // score
                    infoSet.Player infoSet.Deal.Score
            |]
        assert(flags.Length = encodedLength)
        flags
            |> Array.map (function true -> 1f | false -> 0f)
            |> DenseVector.ofArray

    /// "Latin Extended-A" block is printable.
    let private charOffset = 0x100

    /// Converts a byte array to a compact, printable Unicode string.
    let private compact bytes =
        bytes
            |> Array.map (fun (b : byte) ->
                char (int b + charOffset))
            |> String

    /// Converts the given encoding to a string.
    let toString (encoding : Encoding) =
        assert(encoding.Count = encodedLength)
        let bytes =
            encoding.ToArray()
                |> Array.map (function
                    | 0f -> 0uy
                    | 1f -> 1uy
                    | _ -> failwith "Unexpected")
        compact bytes
