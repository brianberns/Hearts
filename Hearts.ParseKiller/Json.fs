namespace Hearts.ParseKiller

open PlayingCards
open Hearts

module Json =

    open System.IO
    open System.Text.Json
    open System.Text.Json.Serialization

    type JsonSerializerOptions with
        member this.GetConverter<'t>() =
            this.GetConverter(typeof<'t>) :?> JsonConverter<'t>

    [<AbstractClass>]
    type WriteOnlyConverter<'t>() =
        inherit JsonConverter<'t>()

        override _.Read(reader, _typeToConvert, _options) =
            failwith "Not implemented"

    type Utf8JsonWriter with
        member writer.WriteArray<'t>(
            items : seq<'t>,
            options : JsonSerializerOptions) =
            let converter = options.GetConverter<'t>()
            writer.WriteStartArray()
            for item in items do
                converter.Write(writer, item, options)
            writer.WriteEndArray()

    type SeatConverter() =
        inherit WriteOnlyConverter<Seat>()
        override _.Write(writer, seat, _options) =
            writer.WriteStringValue($"{Seat.toChar seat}")  

    type CardConverter() =
        inherit WriteOnlyConverter<Card>()
        override _.Write(writer, card, _options) =
            writer.WriteStringValue(
                $"{Rank.toChar card.Rank}{Suit.toLetter card.Suit}")

    type ExchangeConverter() =
        inherit WriteOnlyConverter<Exchange>()
        override _.Write(writer, exchange, options) =

            writer.WriteStartArray()

            for seat, pass in Map.toSeq exchange.PassMap do

                writer.WriteStartObject()

                    // seat passing cards
                writer.WritePropertyName("Seat")
                let converter = options.GetConverter<Seat>()
                converter.Write(writer, seat, options)

                    // cards passed
                writer.WritePropertyName("Pass")
                writer.WriteArray(pass, options)

                writer.WriteEndObject()

            writer.WriteEndArray()

    type TrickConverter() =
        inherit WriteOnlyConverter<Trick>()
        override _.Write(writer, trick, options) =

            writer.WriteStartObject()

                // trick leader
            writer.WritePropertyName("Leader")
            JsonSerializer.Serialize(writer, trick.Leader, options)

                // cards played
            writer.WritePropertyName("Cards")
            writer.WriteArray(List.rev trick.Cards, options)

            writer.WriteEndObject()

    type OpenDealConverter() =
        inherit WriteOnlyConverter<OpenDeal>()

        override _.Write(writer, deal, options) =
            writer.WriteStartObject()

                // exchange direction
            writer.WriteString(
                "PassDirection",
                string deal.ClosedDeal.ExchangeDirection)

                // exchange
            match deal.ExchangeOpt with
                | Some exchange ->
                    writer.WritePropertyName("Passes")
                    let converter = options.GetConverter<Exchange>()
                    converter.Write(writer, exchange, options)
                | None -> ()

                // tricks
            let tricks =
                ClosedDeal.tricks deal.ClosedDeal
                    |> Seq.where (fun trick ->
                            trick.Cards.Length > 0)
            writer.WritePropertyName("Tricks")
            writer.WriteArray(tricks, options)

            writer.WriteEndObject()

    type ScoreConverter() =
        inherit WriteOnlyConverter<Score>()

        override _.Write(writer, score, _options) =
            writer.WriteStartObject()
            for seat in Enum.getValues<Seat> do
                writer.WriteNumber(
                    string (Seat.toChar seat),
                    score[seat])
            writer.WriteEndObject()

    let saveEntries entries =
        let options =
#if DEBUG
            let indent = true
#else
            let indent = false
#endif
            JsonSerializerOptions(WriteIndented = indent)
        for converter in [
            SeatConverter() :> JsonConverter
            CardConverter()
            ExchangeConverter()
            TrickConverter()
            OpenDealConverter()
            ScoreConverter()
        ] do options.Converters.Add(converter)
        use stream = new FileStream("KHearts.json", FileMode.Create)
        JsonSerializer.Serialize(stream, entries, options)
