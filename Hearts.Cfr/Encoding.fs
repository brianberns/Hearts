namespace Hearts.Cfr

open System
open System.Collections

open PlayingCards
open Hearts
open Hearts.Model

module Encoding =

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
                yield! Encoding.encodeCards infoSet.Hand         // current player's hand
                yield! Encoding.encodeCards unseen               // unplayed cards not in current player's hand
                yield! Encoding.encodeTrick trickOpt             // current trick
                yield! Encoding.encodeVoids                      // voids
                    infoSet.Player infoSet.Deal.Voids
                yield! Encoding.encodeScore                      // score
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
