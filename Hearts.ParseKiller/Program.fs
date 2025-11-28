open System
open System.Text

open FParsec

open PlayingCards
open Hearts

let parseSeatChar =
    choice [
        pchar 'W' >>% Seat.West
        pchar 'N' >>% Seat.North
        pchar 'E' >>% Seat.East
        pchar 'S' >>% Seat.South
    ]

let parseDirectionChar =
    choice [
        pchar 'L' >>% ExchangeDirection.Left
        pchar 'R' >>% ExchangeDirection.Right
        pchar 'A' >>% ExchangeDirection.Across
        pchar 'H' >>% ExchangeDirection.Hold
    ]

let parseRankChar =
    choice [
        pchar '2' >>% Rank.Two
        pchar '3' >>% Rank.Three
        pchar '4' >>% Rank.Four
        pchar '5' >>% Rank.Five
        pchar '6' >>% Rank.Six
        pchar '7' >>% Rank.Seven
        pchar '8' >>% Rank.Eight
        pchar '9' >>% Rank.Nine
        pchar 'T' >>% Rank.Ten
        pchar 'J' >>% Rank.Jack
        pchar 'Q' >>% Rank.Queen
        pchar 'K' >>% Rank.King
        pchar 'A' >>% Rank.Ace
    ]

let parseRankChars =
    choice [
        skipChar '-' >>% []
        many1 parseRankChar
    ]

/// E.g. "T9542".
let parseSuitCards suit =
    parse {
        let! ranks = parseRankChars
        do! spaces
        assert(List.distinct ranks = ranks)
        return
            List.map (fun rank ->
                Card.create rank suit) ranks
    }

/// E.g. "W: T9542         654           AQ64          T            ".
let parsePlayerHand seat =
    parse {
        do! skipString $"{Seat.toChar seat}: "
        let! spades = parseSuitCards Suit.Spades
        let! hearts = parseSuitCards Suit.Hearts
        let! clubs = parseSuitCards Suit.Clubs
        let! diamonds = parseSuitCards Suit.Diamonds
        return set [
            yield! spades
            yield! hearts
            yield! clubs
            yield! diamonds
        ]
    }

let parseHands =
    parse {
        do! skipString "spades" >>. spaces
        do! skipString "hearts" >>. spaces
        do! skipString "clubs" >>. spaces
        do! skipString "diamonds" >>. spaces
        let! dealNumber = pint32
        let! dealer = parseSeatChar
        let! dir = parseDirectionChar
        do! skipRestOfLine true
        let! westCards = parsePlayerHand Seat.West
        let! northCards = parsePlayerHand Seat.North
        let! eastCards = parsePlayerHand Seat.East
        let! southCards = parsePlayerHand Seat.South
        let handMap =
            Map [
                Seat.West, westCards
                Seat.North, northCards
                Seat.East, eastCards
                Seat.South, southCards
            ]
        return {|
            DealNumber = dealNumber
            Deal = OpenDeal.fromHands dealer dir handMap
        |}
    }

let parseDeal =
    parseHands

let parseLog =
    many1
        (attempt
            (skipCharsTillString "spades" false Int32.MaxValue
            >>. parseDeal))

let run () =

    Console.OutputEncoding <- Encoding.UTF8

    let result =
        runParserOnFile
            parseLog
            ()
            @"C:\Users\brian\OneDrive\Desktop\KHearts.log"
            Encoding.ASCII

    match result with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

run ()
