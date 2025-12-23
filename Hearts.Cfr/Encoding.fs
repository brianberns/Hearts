namespace Hearts.Cfr

open System
open System.Collections

open PlayingCards
open Hearts
open Hearts.Model

module Encoding =

    /// Total encoded length of an info set.
    let encodedFlagLength =
        Card.numCards                                 // current player's hand
            + Card.numCards                           // unplayed cards not in current player's hand
            + ((Seat.numSeats - 1) * Card.numCards)   // current trick
            + ((Seat.numSeats - 1) * Suit.numSuits)   // voids
            + Seat.numSeats                           // deal score

    /// Encodes the given info set as a vector.
    let encode infoSet : Encoding =

            // encode flags
        let unseen =
            infoSet.Deal.UnplayedCards - infoSet.Hand
        let trickOpt = infoSet.Deal.CurrentTrickOpt
        let flags =
            BitArray [|
                yield! Encoding.encodeCards infoSet.Hand         // current player's hand
                yield! Encoding.encodeCards unseen               // unplayed cards not in current player's hand
                yield! Encoding.encodeTrick trickOpt             // current trick
                yield! Encoding.encodeVoids                      // voids
                    infoSet.Player infoSet.Deal.Voids
                yield! Encoding.encodeDealScore                  // deal score
                    infoSet.Player infoSet.Deal.Score
            |]
        assert(flags.Length = encodedFlagLength)

            // ignore game score
        let gamePoints = Array.zeroCreate Seat.numSeats

        Encoding.create flags gamePoints

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
        assert(encoding.Flags.Length = encodedFlagLength)
        assert(encoding.GamePoints |> Array.forall ((=) 0y))
        let bytes =
            let nBytes = (encoding.Flags.Length + 7) >>> 3
            Array.zeroCreate<byte> nBytes
        encoding.Flags.CopyTo(bytes, 0)
        compact bytes
