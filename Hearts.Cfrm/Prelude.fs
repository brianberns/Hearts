namespace Hearts.Cfrm

module Char =

    /// Converts the given decimal digit to a single character.
    let fromDigit digit =
        "0123456789"[digit]

    /// Converts the given hex digit to a single character.
    let fromHexDigit digit =
        "0123456789ABCDEF"[digit]

module Tuple =

    /// Applies the given function to the items in the given tuple.
    let map f (a, b) =
        (f a), (f b)
