namespace Hearts.ParseKiller

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

open PlayingCards
open Hearts

type LogEntry =
    {
        DealNumber : int
        InitialDeal : OpenDeal
        FinalDeal : OpenDeal
        GameScore : Score
    }

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

[<AbstractClass>]
type WriteOnlyConverter<'t>() =
    inherit JsonConverter<'t>()

    override _.Read(reader, _typeToConvert, _options) =
        failwith "Not implemented"

type SeatConverter() =
    inherit WriteOnlyConverter<Seat>()
    override _.Write(writer, seat, _options) =
        writer.WriteStringValue($"{Seat.toChar seat}")  

type ScoreConverter() =
    inherit WriteOnlyConverter<Score>()

    override _.Write(writer, score, _options) =
        use _ = writer.WriteObject()
        for seat in Enum.getValues<Seat> do
            writer.WriteNumber(
                string (Seat.toChar seat),
                score[seat])

type CardConverter() =
    inherit WriteOnlyConverter<Card>()
    override _.Write(writer, card, _options) =
        writer.WriteStringValue(
            $"{Rank.toChar card.Rank}{Suit.toLetter card.Suit}")

type ExchangeConverter() =
    inherit WriteOnlyConverter<Exchange>()
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

type TrickConverter() =
    inherit WriteOnlyConverter<Trick>()
    override _.Write(writer, trick, options) =
        use _ = writer.WriteObject()

            // trick leader
        writer.WritePropertyName("Leader")
        writer.Write(trick.Leader, options)

            // cards played
        writer.WritePropertyName("Cards")
        writer.WriteArray(List.rev trick.Cards, options)

type LogEntryConverter() =
    inherit WriteOnlyConverter<LogEntry>()

    override _.Write(writer, entry, options) =
        use _ = writer.WriteObject()

            // deal number
        writer.WriteNumber(
            "DealNumber", entry.DealNumber)

            // initial hands


            // exchange direction
        writer.WriteString(
            "PassDirection",
            string entry.InitialDeal.ClosedDeal.ExchangeDirection)

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

module Json =

    let saveEntries path (entries : seq<LogEntry>) =
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
            ExchangeConverter()
            TrickConverter()
            LogEntryConverter()
        ] do options.Converters.Add(converter)
        use stream = new FileStream(path, FileMode.Create)
        JsonSerializer.Serialize(stream, entries, options)
