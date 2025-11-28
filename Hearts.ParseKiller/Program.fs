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

let parseSuitChar =
    choice [
        pchar 'S' >>% Suit.Spades
        pchar 'H' >>% Suit.Hearts
        pchar 'C' >>% Suit.Clubs
        pchar 'D' >>% Suit.Diamonds
    ]

/// E.g. "T9542".
let parseRankChars =
    choice [
        skipChar '-' >>% []
        many1 parseRankChar
    ]

/// E.g. "T9542         ".
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

(*
 *    spades        hearts        clubs         diamonds   439063SL 0 0 0 0
 * W: T9542         654           AQ64          T            
 * N: -             KQ97          KT87532       A4           
 * E: AKJ           82            J             KQJ9863      
 * S: Q8763         AJT3          9             752          
 *)
let parseInitialDeal =
    parse {
        do! skipString "spades" >>. spaces
        do! skipString "hearts" >>. spaces
        do! skipString "clubs" >>. spaces
        do! skipString "diamonds" >>. spaces
        let! _dealNumber = pint32   // ignore for now
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
        return OpenDeal.fromHands dealer dir handMap
    }

let parseCard =
    parse {
        let! rank = parseRankChar
        let! suit = parseSuitChar
        return Card.create rank suit
    }

let parsePlayerPasses =
    parse {
        let! seat = parseSeatChar
        do! skipString "->"
        let! cardA = parseCard
        let! cardB = parseCard
        let! cardC = parseCard
        let cards = [ cardA; cardB; cardC ]
        assert(List.distinct cards = cards)
        return seat, cards
    }

let parseAutoPasses (dir : ExchangeDirection) =
    parse {
        do! skipString $"{dir.ToString().ToLower()} auto passes: "
        let! playerPasses =
            sepBy parsePlayerPasses (skipChar ' ')
        do! skipNewline
        return Map playerPasses
    }

let applyPasses deal (autoPasses : Map<_, _>) =
    let cards =
        let seats =
            Seat.cycle (OpenDeal.currentPlayer deal)
        seq {
            for seat in seats do
                yield! autoPasses[seat]
        }
    (deal, cards)
        ||> Seq.fold (fun deal card ->
            OpenDeal.addPass card deal)
        |> OpenDeal.startPlay

let parseDeal =
    parse {
        let! deal = parseInitialDeal
        let isHold =
            deal.ClosedDeal.ExchangeDirection = ExchangeDirection.Hold
        if isHold then
            return deal
        else
            let! passes =
                parseAutoPasses deal.ClosedDeal.ExchangeDirection
            let deal = applyPasses deal passes
            do! skipNewline
            do! spaces
            let! dealCheck = parseInitialDeal
            assert(dealCheck.UnplayedCardMap = deal.UnplayedCardMap)
            return deal
    }

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
