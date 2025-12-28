namespace Hearts.Cfr

open System

open MathNet.Numerics.LinearAlgebra

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
        let flags =
            [|
                yield! Encoding.encodeCards infoSet.Hand         // current player's hand
                yield! Encoding.encodeCards unseen               // unplayed cards not in current player's hand
                yield! Encoding.encodeTrick trickOpt             // current trick
                yield! Encoding.encodeVoids                      // voids
                    infoSet.Player infoSet.Deal.Voids
                yield! Encoding.encodeScore                      // score
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
