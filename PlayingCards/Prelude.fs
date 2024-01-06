namespace PlayingCards

open System

module Enum =

    /// Answers all values of the given enum type.
    let inline getValues<'enum> =
        Enum.GetValues(typeof<'enum>)
            |> Seq.cast<'enum>
            |> Seq.toArray

module Array =

    /// Clones the given array.
    let clone (items : 'item[]) =
#if FABLE_COMPILER
        items
            |> Seq.readonly   // force a copy
            |> Seq.toArray
#else
        items.Clone()
            :?> 'item[]
#endif

    /// Shuffles the given array in place.
    /// From http://rosettacode.org/wiki/Knuth_shuffle#F.23
    let shuffle (rng : Random) (items : _[]) =
        let swap i j =
            let item = items.[i]
            items.[i] <- items.[j]
            items.[j] <- item
        let len = items.Length
        [0 .. len - 2]
            |> Seq.iter (fun i -> swap i (rng.Next(i, len)))
        items

/// Option computation expression builder.
type OptionBuilder() =
    member _.Bind(opt, f) = Option.bind f opt
    member _.Return(x) = Some x
    member _.ReturnFrom(opt : Option<_>) = opt
    member _.Zero() = None

[<AutoOpen>]
module OptionBuilder =

    /// Option computation expression builder.
    let option = OptionBuilder()
