﻿namespace Hearts

open PlayingCards

/// A location occupied by a player.
type Seat =
    | West  = 0
    | North = 1
    | East  = 2
    | South = 3

module Seat =

    /// Total number of seats.
    let numSeats =
        Enum.getValues<Seat>.Length

    /// Converts the given seat to a character.
    let toChar (seat : Seat) =
        "WNES"[int seat]

#if FABLE_COMPILER
    /// Display name.
    let toString = function
        | Seat.West -> "West"
        | Seat.North -> "North"
        | Seat.East -> "East"
        | Seat.South -> "South"
        | _ -> failwith "Unexpected seat"
#endif

    /// Converts the given character to a seat.
    let fromChar = function
        | 'W' -> Seat.West
        | 'N' -> Seat.North
        | 'E' -> Seat.East
        | 'S' -> Seat.South
        | _ -> failwith "Unexpected seat"

    /// Nth seat after the given seat.
    let incr n (seat : Seat) =
        assert(n >= 0)
        (int seat + n) % numSeats
            |> enum<Seat>

    /// Seat that plays after the given seat.
    let next = incr 1

    /// All seats in order starting with the given seat.
    let cycle seat =
        seq {
            for i = 0 to numSeats - 1 do
                yield seat |> incr i
        }

    /// Answers the non-negative offset of the given seat relative
    /// to the given base seat.
    let getIndex (seat : Seat) (baseSeat : Seat) =
        let idx = ((int seat) - (int baseSeat) + numSeats) % numSeats
        assert(idx >= 0)
        assert(idx < numSeats)
        idx
