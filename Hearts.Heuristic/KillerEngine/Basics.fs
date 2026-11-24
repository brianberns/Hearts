/// Killer Hearts' vocabulary: seats, suits and ranks as small integers, in Killer Hearts' own orders,
/// for its iteration order decides many ties.
[<AutoOpen>]
module Hearts.Heuristic.KillerEngine.Basics

open PlayingCards

/// A seat: west = 0, north = 1, east = 2, south = 3, in playing order.
type KSeat = int

/// A suit: spades = 0, hearts = 1, clubs = 2, diamonds = 3.
type KSuit = int

/// A rank: 2 to 14 (ace).
type KRank = int

let spades, hearts, clubs, diamonds = 0, 1, 2, 3
let jack, queen, king, ace = 11, 12, 13, 14

/// Killer Hearts' "illegal_value", its marker for "none".
let none = -1

let allSeats = [ 0 .. 3 ]
let allSuits = [ spades .. diamonds ]

let nextSeat (seat : KSeat) = (seat + 1) % 4
let previousSeat (seat : KSeat) = (seat + 3) % 4

/// Converts a suit to Killer Hearts' numbering, and back.
let toKSuit (suit : Suit) =
    match suit with
        | Suit.Spades -> spades
        | Suit.Hearts -> hearts
        | Suit.Clubs -> clubs
        | _ -> diamonds

let private fromKSuits = [| Suit.Spades; Suit.Hearts; Suit.Clubs; Suit.Diamonds |]
let fromKSuit (suit : KSuit) = fromKSuits[suit]

/// A card as Killer Hearts sees it.
[<Struct>]
type KCard =
    { Rank : KRank; Suit : KSuit }
    static member Of(card : Card) = { Rank = int card.Rank; Suit = toKSuit card.Suit }
    member c.ToCard() = Card(enum<Rank> c.Rank, fromKSuit c.Suit)
    member c.IsQS = c.Rank = queen && c.Suit = spades
    member c.Points = if c.Suit = hearts then 1 elif c.IsQS then 13 else 0
    /// Killer Hearts' card number, 4 * (rank - 2) + suit.
    member c.Index = 4 * (c.Rank - 2) + c.Suit
    override c.ToString() = string (c.ToCard())

/// Pass directions, in Killer Hearts' numbering.
let passLeft, passRight, passAcross, hold = 0, 1, 2, 3

/// The seat a seat passes to.
let passTarget direction (seat : KSeat) =
    (seat + [| 1; 3; 2; 0 |][direction]) % 4

/// The seat a seat receives its pass from.
let passSource direction (seat : KSeat) =
    (seat - [| 1; 3; 2; 0 |][direction] + 4) % 4

/// Killer Hearts' modes of play.
type Mode =
    | NoMode = 0
    | ShootTheMoon = 1
    | StopShoot = 2
    | GetLowScorer = 3
    | BeLowScorer = 4
    | LieLow = 5
    | Equivocate = 6
    | HelpShooter = 7
    | ProtectHiScorer = 8

/// Who holds the points played so far: no one yet, more than one seat, or one seat.
module PointsSit =
    let unplayed = -1
    let split = -2

/// Inference codes, as Killer Hearts records knowledge of a seat's cards. The low four bits are the
/// certainty (15 is certain); codes from 512 to 767 mean the seat does not hold the card, codes from
/// 1024 up mean it does, and 0 means nothing is known.
module Info =
    let certainty code = code &&& 15
    let isHeld code = code >= 1024
    let isAbsent code = code >= 512 && code < 1024
    /// The seat may hold the card: it is known or inferred to, or nothing is known.
    let mayHold code = code >= 1024 || code = 0

/// Ways Killer Hearts V8 (Killer.fs) departs from Killer Hearts V7.2, inside the engine.
type Options =
    {
        /// Never try to Shoot.
        NoShoot : bool

        /// Added to the strength needed to try to Shoot, when passing and playing: below 0 tries more
        /// readily, above 0 less.
        ShootShift : int
    }

module Options =

    /// Killer Hearts V7.2, exactly.
    let v72 = { NoShoot = false; ShootShift = 0 }

/// Game scores as Killer Hearts expects them: none 100 or more before a deal. A game that continues
/// after a tie at 100 or more (Rules.arena) would otherwise be out of Killer Hearts' range, so the scores
/// are shifted down until the highest is 99, keeping their differences (as the arena bot does).
let killerScores (scores : int[]) =
    let shift = max (Array.max scores - 99) 0
    scores |> Array.map (fun s -> min 99 (max 0 (s - shift)))

/// Sorts four values ascending, as Killer Hearts' indirect_sort does; index 0 is lowest.
let sortedScores (scores : int[]) = Array.sort scores

/// Killer Hearts' constant tables.
module Tables =

    /// Estimated tricks a seat can lead in a suit before the opponents run out of it, by the number
    /// of opponents void in it (0 or 1) and the number of cards they hold, on a passing hand and on a
    /// hold hand (est_t_p, est_t_h).
    let estTricksPass =
        array2D [ [ 0; 1; 1; 2; 3; 3; 3; 4; 4; 5; 6; 6; 6; 6 ]
                  [ 0; 1; 2; 2; 3; 3; 4; 5; 6; 6; 6; 7; 7; 7 ] ]
    let estTricksHold =
        array2D [ [ 0; 1; 1; 2; 2; 3; 3; 3; 4; 4; 5; 5; 5; 6 ]
                  [ 0; 1; 1; 2; 3; 4; 4; 5; 5; 6; 6; 6; 7; 7 ] ]
