open System
open System.Text

open FParsec

open PlayingCards
open Hearts

let parseSeat =
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
            many1 parseRankChar
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
            let! dealNum = pint32   // ignore deal number
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
                DealNumber = dealNum
                OpenDeal = OpenDeal.fromHands dealer dir handMap
            |}
        }

module Exchange =

    /// E.g. "W->TD6HAC".
    let private parsePlayerPasses =
        parse {
            let! seat = parseSeatChar
            do! skipString "->"
            let! cards = many1 parseCard
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

    let private apply deal (passMap : Map<_, _>) =
        let cards =
            let seats =
                Seat.cycle (OpenDeal.currentPlayer deal)
            seq {
                for seat in seats do
                    yield! passMap[seat]
            }
        (deal, cards)
            ||> Seq.fold (fun deal card ->
                OpenDeal.addPass card deal)

    let parse deal =
        let isHold =
            deal.ClosedDeal.ExchangeDirection
                = ExchangeDirection.Hold
        parse {
            if isHold then
                return OpenDeal.startPlay deal
            else
                    // parse and apply the auto passes
                let! passes =
                    parseAutoPasses deal.ClosedDeal.ExchangeDirection
                let deal = apply deal passes
                let deal = OpenDeal.startPlay deal

                    // verify exchange
                do! skipNewline
                do! spaces
                let! afterDeal = InitialDeal.parse
                assert(
                    afterDeal.OpenDeal.UnplayedCardMap
                        = deal.UnplayedCardMap)

                return deal
        }

module Playout =

    /// E.g. "W:TS".
    let parsePlay =
        parse {
            let! seat = parseSeatChar
            do! skipChar ':'
            let! card = parseCard
            return seat, card
        }

    let private apply deal tricks =
        let plays = List.concat tricks
        (deal, plays)
            ||> Seq.fold (fun deal (seat, card) ->
                assert(OpenDeal.currentPlayer deal = seat)
                OpenDeal.addPlay card deal)

    /// E.g. "trick  1: W:TS  N:KH  E:AS  S:3D".
    let private parseTrick =
        parse {
            do! skipString "trick"
            do! spaces
            let! _ = pint32   // ignore trick number
            do! skipString ": "
            let! plays = sepBy parsePlay (pstring "  ")
            assert(plays.Length = Seat.numSeats)
            return plays
        }

    let parse deal =
        parse {
            let! tricks = many1 (parseTrick .>> newline)
            return apply deal tricks
        }

module GameScore =

    let private parsePlayerPoints =
        parse {
            let! seat = parseSeatChar
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

let skipEndOfGameComment =
    parse {
        do! skipString "End of game:"
        do! skipRestOfLine true
    }

let skipShootComment =
    parse {
        do! parseSeat |>> ignore
        do! skipString " Shoot"
        do! skipRestOfLine true
    }

let parseDeal =
    parse {
        let! killerDeal = InitialDeal.parse
        let! openDeal = Exchange.parse killerDeal.OpenDeal
        let! openDeal = Playout.parse openDeal
        do! optional skipEndOfGameComment
        do! optional skipShootComment
        let! score = GameScore.parse   // game score after deal is complete
        return {|
            DealNumber = killerDeal.DealNumber
            OpenDeal = openDeal
            GameScore = score
        |}
    }

let parseEntry =
    parse {
        do! skipCharsTillString "spades" false Int32.MaxValue
        return! parseDeal
    } |> attempt

let parseLog =
    parse {
        // let! entries = many1 parseEntry
        let! entries = parseEntry |>> List.singleton
        return (Score.zero, entries)
            ||> Seq.mapFold (fun score entry ->
                {| entry with GameScore = score |},   // game score at start of deal
                entry.GameScore)
            |> fst
    }

module Json =

    open System.IO
    open System.Text.Json
    open System.Text.Json.Serialization

    [<AbstractClass>]
    type WriteOnlyConverter<'t>() =
        inherit JsonConverter<'t>()
        override _.Read(reader, _typeToConvert, _options) =
            failwith "Not implemented"

    type CardConverter() =
        inherit WriteOnlyConverter<Card>()
        override _.Write(writer, card, _options) =
            writer.WriteStringValue(
                $"{Rank.toChar card.Rank}{Suit.toLetter card.Suit}")

    type TrickConverter() =
        inherit WriteOnlyConverter<Trick>()
        override _.Write(writer, trick, options) =

            writer.WriteStartObject()

                // leader
            writer.WriteString("Leader", $"Seat.toChar trick.Leader")

                // cards
            writer.WriteStartArray("Cards")
            for card in List.rev trick.Cards do
                JsonSerializer.Serialize(writer, card, options)
            writer.WriteEndArray()

            writer.WriteEndObject()

    let saveEntries entries =
        let options =
            JsonSerializerOptions(WriteIndented = true)
        for converter in [
            JsonStringEnumConverter() :> JsonConverter
            CardConverter()
            TrickConverter()
        ] do options.Converters.Add(converter)
        use stream = new FileStream("KHearts.json", FileMode.Create)
        JsonSerializer.Serialize(stream, entries, options)

let run () =

    Console.OutputEncoding <- Encoding.UTF8

    let result =
        runParserOnFile
            parseLog
            ()
            @"C:\Users\brian\OneDrive\Desktop\KHearts.log"
            Encoding.ASCII

    match result with
        | Success(entries, _, _)   -> Json.saveEntries entries
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

run ()
