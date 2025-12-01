namespace Hearts.ParseKiller

open System
open System.Text

open FParsec

open PlayingCards
open Hearts

module Killer =

    let parseSeat : Parser<_, unit> =
        choice [
            pstring "West" >>% Seat.West
            pstring "North" >>% Seat.North
            pstring "East" >>% Seat.East
            pstring "South" >>% Seat.South
        ]

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

    let parseCard =
        parse {
            let! rank = parseRankChar
            let! suit = parseSuitChar
            return Card.create rank suit
        }

module InitialDeal =

    /// E.g. "T9542".
    let private parseRankChars =
        choice [
            skipChar '-' >>% []
            many1 Killer.parseRankChar
        ]

    /// E.g. "T9542         ".
    let private parseSuitCards suit =
        parse {
            let! ranks = parseRankChars
            do! spaces
            assert(List.distinct ranks = ranks)
            return
                List.map (fun rank ->
                    Card.create rank suit) ranks
        }

    /// E.g. "W: T9542         654           AQ64          T            ".
    let private parsePlayerHand seat =
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
    let parse =
        parse {
            do! skipString "spades" >>. spaces
            do! skipString "hearts" >>. spaces
            do! skipString "clubs" >>. spaces
            do! skipString "diamonds" >>. spaces
            let! dealNum = pint32
            let! dealer = Killer.parseSeatChar
            let! dir = Killer.parseDirectionChar
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
            let deal = OpenDeal.fromHands dealer dir handMap
            return dealNum, deal
        }

module Exchange =

    /// E.g. "W->TD6HAC".
    let private parsePlayerPasses =
        parse {
            let! seat = Killer.parseSeatChar
            do! skipString "->"
            let! cards = many1 Killer.parseCard
            assert(cards.Length = Pass.numCards)
            assert(List.distinct cards = cards)
            return seat, cards
        }

    /// E.g. "left auto passes: W->TD6HAC N->KHQH9H E->ASJC8H S->AHJHTH".
    let private parseAutoPasses (dir : ExchangeDirection) =
        parse {
            do! skipString $"{dir.ToString().ToLower()} auto passes: "
            let! playerPasses =
                sepBy parsePlayerPasses (skipChar ' ')
            do! skipNewline
            return Map playerPasses
        }

    let parseAndApply deal =
        let isHold =
            deal.ClosedDeal.ExchangeDirection
                = ExchangeDirection.Hold
        parse {
            if isHold then
                return OpenDeal.startPlay deal
            else
                    // parse and apply the auto passes
                let! passMap =
                    parseAutoPasses deal.ClosedDeal.ExchangeDirection
                let deal = PassMap.apply passMap deal
                let deal = OpenDeal.startPlay deal

                    // verify exchange
                do! skipNewline
                do! spaces
                let! _, afterDeal = InitialDeal.parse
                assert(
                    afterDeal.UnplayedCardMap
                        = deal.UnplayedCardMap)

                return deal
        }

module Playout =

    /// E.g. "W:TS".
    let parsePlay =
        parse {
            let! seat = Killer.parseSeatChar
            do! skipChar ':'
            let! card = Killer.parseCard
            return seat, card
        }

    /// E.g. "trick  1: W:TS  N:KH  E:AS  S:3D".
    let private parseTrickPlays =
        parse {
            do! skipString "trick"
            do! spaces
            let! _ = pint32   // ignore trick number
            do! skipString ": "
            let! plays = sepBy parsePlay (pstring "  ")
            assert(plays.Length = Seat.numSeats)
            return plays
        }

    let parseAndApply deal =
        parse {
            let! plays =
                many1 (parseTrickPlays .>> newline)
                    |>> List.concat
            return TrickPlay.apply plays deal
        }

module GameScore =

    let private parsePlayerPoints =
        parse {
            let! seat = Killer.parseSeatChar
            do! skipString " = "
            let! points = pint32
            return seat, points
        }

    let parse =
        parse {
            let! playerPoints =
                many1 (parsePlayerPoints .>> skipChar ' ')
            do! skipRestOfLine true
            return playerPoints
                |> Seq.sortBy fst
                |> Seq.map snd
                |> Seq.toArray
                |> Score.ofPoints
        }

module Log =

    let private skipEndOfGameComment =
        parse {
            do! skipString "End of game:"
            do! skipRestOfLine true
        }

    let private skipShootComment =
        parse {
            do! Killer.parseSeat |>> ignore
            do! skipString " Shoot"
            do! skipRestOfLine true
        }

    let private parseLogEntry =
        parse {
            let! dealNum, initialDeal = InitialDeal.parse
            let! finalDeal =
                initialDeal
                    |> Exchange.parseAndApply 
                    >>= Playout.parseAndApply
            do! optional skipEndOfGameComment
            do! optional skipShootComment
            let! finalScore = GameScore.parse   // game score after deal is complete
            return {
                DealNumber = dealNum
                InitialDeal = initialDeal
                FinalDeal = finalDeal
                GameScore = finalScore
            }
        }

    let private skipToAndParseEntry =
        parse {
            do! skipCharsTillString "spades" false Int32.MaxValue
            return! parseLogEntry
        } |> attempt

    let private parseEntries =
        parse {
            let! entries = many1 skipToAndParseEntry
            // let! entries = skipToAndParseEntry |>> List.singleton
            return (Score.zero, entries)
                ||> List.mapFold (fun score entry ->
                    { entry with GameScore = score },   // game score at start of deal
                    entry.GameScore)
                |> fst
        }

    let convertToJson inputPath outputPath =

        let result =
            runParserOnFile
                parseEntries () inputPath Encoding.ASCII

        match result with
            | Success(entries, _, _) ->
                let distinct = List.distinct entries
                printfn $"{entries.Length} entries parsed ({distinct.Length} distinct)"
                Json.saveEntries outputPath distinct
            | Failure(errorMsg, _, _) ->
                printfn "Failure: %s" errorMsg
