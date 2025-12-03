namespace Hearts.ParseKiller

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

open PlayingCards
open Hearts

/// Entry in a log file describing a Hearts deal.
type LogEntry =
    {
        /// Killer Hearts deal number.
        DealNumber : int

        /// Game score at the beginning of this deal.
        GameScore : Score

        /// Initial state of deal before exchange and playout.
        InitialDeal : OpenDeal

        /// Final state of deal after exchange and playout.
        FinalDeal : OpenDeal
    }

type LogEntryLite =
    {
        /// Game score at the beginning of this deal.
        GameScore : Score

        /// Initial state of deal before exchange and playout.
        InitialDeal : OpenDeal

        /// Exchange, if a non-hold deal.
        ExchangeOpt : Option<Exchange>

        /// Tricks played, in chronological order.
        Tricks : Trick[]
    }

/// JSON serializer extensions.
[<AutoOpen>]
module JsonExt =

    type JsonSerializerOptions with

        member this.GetConverter<'t>() =
            this.GetConverter(typeof<'t>) :?> JsonConverter<'t>

    type Utf8JsonWriter with

        member this.Write(value, options : JsonSerializerOptions) =
            let converter = options.GetConverter()
            converter.Write(this, value, options)

        member writer.WriteObject() =
            writer.WriteStartObject()
            {
                new IDisposable with
                    member _.Dispose() =
                        writer.WriteEndObject()
            }

        member writer.WriteArray() =
            writer.WriteStartArray()
            {
                new IDisposable with
                    member _.Dispose() =
                        writer.WriteEndArray()
            }

        member writer.WriteArray(
            items, options : JsonSerializerOptions) =
            use _ = writer.WriteArray()
            let converter = options.GetConverter()
            for item in items do
                converter.Write(writer, item, options)

/// Seat.
type SeatConverter() =
    inherit JsonConverter<Seat>()

    override _.Write(writer, seat, _options) =
        writer.WriteStringValue($"{Seat.toChar seat}")

    static member ReadChar(reader : Utf8JsonReader) =
        reader.GetString()
            |> Seq.exactlyOne
            |> Seat.fromChar

    override _.Read(reader, _typeToConvert, _options) =
        SeatConverter.ReadChar(reader)

    override _.ReadAsPropertyName(
        reader, _typeToConvert, _options) =
        SeatConverter.ReadChar(reader)

/// Score.
type ScoreConverter() =
    inherit JsonConverter<Score>()

    override _.Write(writer, score, _options) =
        use _ = writer.WriteObject()
        for seat in Enum.getValues<Seat> do
            writer.WriteNumber(
                string (Seat.toChar seat),
                score[seat])

    override _.Read(reader, _typeToConvert, options) =
        JsonSerializer.Deserialize<Map<Seat, int>>(
            &reader, options)
            |> Map.values   // sorted by seat
            |> Seq.toArray
            |> Score.ofPoints

/// Card.
type CardConverter() =
    inherit JsonConverter<Card>()

    override _.Write(writer, card, _options) =
        writer.WriteStringValue(
            $"{Rank.toChar card.Rank}{Suit.toLetter card.Suit}")

    override _.Read(reader, _typeToConvert, _options) =
        reader.GetString()
            |> Card.fromString

/// Hand.
type HandConverter() =
    inherit JsonConverter<Hand>()

    override _.Write(writer, hand, _options) =

            // organize cards by suit
        let suitRanks =
            hand
                |> Seq.groupBy _.Suit
                |> Seq.sortByDescending fst    // sort suits high to low
                |> Seq.map (fun (suit, cards) ->
                    suit,
                    cards
                        |> Seq.sortDescending   // sort ranks high to low
                        |> Seq.map (fun card -> Rank.toChar card.Rank)
                        |> Seq.toArray
                        |> String)

        use _ = writer.WriteObject()
        for suit, ranksStr in suitRanks do
            writer.WritePropertyName($"{Suit.toLetter suit}")
            writer.WriteStringValue(ranksStr)

    override _.Read(reader, _typeToConvert, options) =
        JsonSerializer.Deserialize<Map<string, string>>(
            &reader, options)
            |> Map.toSeq
            |> Seq.collect (fun (suitLetter, ranksStr) ->
                let suit =
                    suitLetter
                        |> Seq.exactlyOne
                        |> Suit.fromChar
                ranksStr
                    |> Seq.map (fun rankChar ->
                        let rank = Rank.fromChar rankChar
                        Card.create rank suit))
            |> set

/// Exchange.
type ExchangeConverter() =
    inherit JsonConverter<Exchange>()

    override _.Write(writer, exchange, options) =
        use _ = writer.WriteArray()

        for seat, pass in Map.toSeq exchange.PassMap do
            use _ = writer.WriteObject()

                // seat passing cards
            writer.WritePropertyName("Seat")
            writer.Write(seat, options)

                // cards passed
            writer.WritePropertyName("Pass")
            writer.WriteArray(pass, options)

    override _.Read(reader, _typeToConvert, options) =
        let maps =
            JsonSerializer.Deserialize<Map<string, JsonElement>[]>(&reader)
        let passMap =
            Map [
                for map in maps do
                    let seat = map["Seat"].Deserialize<Seat>(options)
                    let cards = map["Pass"].Deserialize<Card[]>(options)   // can't deserialize directly to Pass, because it's idential to Hand
                    seat, set cards
            ]
        let exchange =
            {
                CurrentPasserOpt = None
                PassMap = passMap
            }
        assert(Exchange.isComplete exchange)
        exchange

/// Trick.
type TrickConverter() =
    inherit JsonConverter<Trick>()

    override _.Write(writer, trick, options) =
        use _ = writer.WriteObject()

            // trick leader
        writer.WritePropertyName("Leader")
        writer.Write(trick.Leader, options)

            // cards played
        writer.WritePropertyName("Cards")
        writer.WriteArray(List.rev trick.Cards, options)

    override _.Read(reader, _typeToConvert, options) =
        let map =
            JsonSerializer.Deserialize<Map<string, JsonElement>>(&reader)
        let leader = map["Leader"].Deserialize<Seat>(options)
        let cards = map["Cards"].Deserialize<Card[]>(options)
        (Trick.create leader, cards)
            ||> Seq.fold (fun trick card ->
                Trick.addPlay card trick)

/// Log entry.
type LogEntryConverter() =
    inherit JsonConverter<LogEntry>()

    override _.Write(writer, entry, options) =
        use _ = writer.WriteObject()

            // deal number
        writer.WriteNumber(
            "DealNumber", entry.DealNumber)

            // dealer
        writer.WritePropertyName("Dealer")
        writer.Write(entry.InitialDeal.ClosedDeal.Dealer, options)

            // initial score
        writer.WritePropertyName("Score")
        writer.Write(entry.GameScore, options)

            // initial hands
        let seatHands =
            Map.toSeq entry.InitialDeal.UnplayedCardMap
        writer.WritePropertyName("Hands")
        do
            use _ = writer.WriteObject()
            for seat, hand in seatHands do
                writer.WritePropertyName($"{Seat.toChar seat}")
                writer.Write(hand, options)

            // exchange direction
        writer.WriteString(
            "PassDirection",
            string entry.InitialDeal.ClosedDeal.ExchangeDirection)   // can't use JsonStringEnumConverter?

            // exchange
        match entry.FinalDeal.ExchangeOpt with
            | Some exchange ->
                writer.WritePropertyName("Passes")
                writer.Write(exchange, options)
            | None -> ()

            // tricks
        let tricks =
            ClosedDeal.tricks entry.FinalDeal.ClosedDeal
                |> Seq.where (fun trick ->
                        trick.Cards.Length > 0)
        writer.WritePropertyName("Tricks")
        writer.WriteArray(tricks, options)

    override _.Read(reader, _typeToConvert, options) =
        raise (NotSupportedException())

/// Log entry lite.
type LogEntryLiteConverter() =
    inherit JsonConverter<LogEntryLite>()

    override _.Write(writer, entry, options) =
        raise (NotSupportedException())

    override _.Read(reader, _typeToConvert, options) =

        let map =
            JsonSerializer.Deserialize<Map<string, JsonElement>>(&reader)

            // initial score
        let gameScore = map["Score"].Deserialize<Score>(options)

            // initial hands
        let handMap =
            map["Hands"].Deserialize<Map<Seat, Hand>>(options)

            // exchange direction
        let dir =
            map["PassDirection"].GetString()
                |> Enum.Parse<ExchangeDirection>   // can't use JsonStringEnumConverter?

            // exchange
        let exchangeOpt =
            if dir = ExchangeDirection.Hold then None
            else
                Some (map["Passes"].Deserialize<Exchange>(options))

            // tricks
        let tricks =
            map["Tricks"].Deserialize<Trick[]>(options)

            // construct entry
        let initialDeal =
            let dealer = map["Dealer"].Deserialize<Seat>(options)
            OpenDeal.fromHands dealer dir handMap
        {
            GameScore = gameScore
            InitialDeal = initialDeal
            ExchangeOpt = exchangeOpt
            Tricks = tricks
        }

module Json =

    let private createOptions () =

        let options =
#if DEBUG
            let indent = true
#else
            let indent = false
#endif
            JsonSerializerOptions(WriteIndented = indent)

        for converter in [
            SeatConverter() :> JsonConverter
            ScoreConverter()
            CardConverter()
            HandConverter()
            ExchangeConverter()
            TrickConverter()
            LogEntryConverter()
            LogEntryLiteConverter()
        ] do options.Converters.Add(converter)

        options

    let saveEntries path (entries : seq<LogEntry>) =
        let options = createOptions ()
        use stream = new FileStream(path, FileMode.Create)
        JsonSerializer.Serialize(stream, entries, options)

    let loadEntries path =
        let options = createOptions ()
        use stream = new FileStream(path, FileMode.Open)
        JsonSerializer.Deserialize<LogEntryLite[]>(stream, options)
