namespace Hearts.PlayKiller

open System
open System.IO
open System.Text
open System.Threading

open PlayingCards
open Hearts

type ServerRecordType =
    | SessionStart = 1
    | GameStart = 2
    | DealStart = 3
    | Hand = 4
    | ExchangeOutgoing = 5
    | ExchangeIncoming = 6
    | TrickStart = 7
    | Play = 8
    | TrickFinish = 9
    | DealFinish = 10

type SessionStartRecord =
    {
        ClientSeats : Set<Seat>
        HostDisplay : bool
        TwoOfClubsLeads : bool
    }

type GameStartRecord =
    {
        Dealer : Seat
        DealNum : int
        NumGames : int
    }

type DealStartRecord =
    {
        GameScore : Score
        ExchangeDirection : ExchangeDirection
    }

type CardsRecord =
    {
        Seat : Seat
        Cards : Set<Card>
    }

type TrickStartRecord =
    {
        Leader : Seat
        TrickNum : int
    }

type ClientRecordType =
    | SessionStart = 101
    | GameStart = 102
    | DealStart = 103
    | Hand = 104
    | ExchangeOutgoing = 105
    | ExchangeIncoming = 106
    | TrickStart = 107
    | Play = 108
    | TrickFinish = 109
    | DealFinish = 110

/// Low-level protocol for interacting with Killer Hearts.
module Protocol =

    /// File used to send messages in both directions.
    let sharedFile =
        File.Open(
            "C:\Program Files\KHearts\KH_Host_Client.dat",
            FileMode.Open,
            FileAccess.ReadWrite,
            FileShare.ReadWrite)

    /// Buffer used for reading messages.
    let buffer = Array.zeroCreate<byte> 55

    /// Reads a message of the given type from KH.
    let private read (recordType : ServerRecordType) =

        let rec loop sleep =
            Thread.Sleep(sleep : int)
            sharedFile.Seek(0L, SeekOrigin.Begin) |> ignore
            let nBytes = sharedFile.Read(buffer, 0, buffer.Length)
            assert(nBytes = buffer.Length)
            let str = Encoding.Default.GetString(buffer)
            let chunks = str.Split(',')
            if chunks.[0] = "HCs" && chunks.[7] = "HCe" then
                printfn $"read:  |{String.Join(',', chunks.[1..6])}|"
                chunks.[1..6]
            elif sleep > 2000 then
                failwithf $"Incorrect header/trailer: {chunks.[0]}/{chunks.[7]}"
            else loop (2 * sleep)

        let fields = loop 1
        if Int32.Parse(fields.[0]) <> int recordType then
            failwith $"Incorrect key: expected {int recordType}, actual {fields.[0]}"
        fields

    /// Reads and ignores a message of the given type from KH.
    let readIgnore recordType =
        read recordType |> ignore

    /// Reads a session start record.
    let readSessionStart () =
        let fields = read ServerRecordType.SessionStart
        {
            ClientSeats =
                let field = fields.[1] |> Int32.Parse
                Seat.allSeats
                    |> Seq.where (fun seat ->
                        (field >>> int seat) &&& 1 = 1)
                    |> set
            HostDisplay =
                Int32.Parse(fields.[4]) = 1
            TwoOfClubsLeads =
                Int32.Parse(fields.[5]) = 0
        }

    /// Reads a game start record.
    let readGameStart () =
        let fields = read ServerRecordType.GameStart
        {
            Dealer =
                fields.[1]
                    |> Int32.Parse
                    |> enum<Seat>
            DealNum = fields.[2] |> Int32.Parse
            NumGames = fields.[4] |> Int32.Parse
        }

    /// Reads a deal start record.
    let readDealStart () =
        let fields = read ServerRecordType.DealStart
        {
            GameScore =
                Seat.allSeats
                    |> Seq.map (fun seat ->
                        let points =
                            fields.[int seat + 1] |> Int32.Parse
                        seat, points)
                    |> Map
                    |> ScoreMap
            ExchangeDirection =
                match fields.[5] |> Int32.Parse with
                    | 0 -> ExchangeDirection.Left
                    | 1 -> ExchangeDirection.Right
                    | 2 -> ExchangeDirection.Across
                    | 3 -> ExchangeDirection.Hold
                    | _ -> failwith "Unexpected"
        }

    /// Reads a hand.
    let readHand () =

        let readRanks (field : int) =
            [|
                for rank in Enum.getValues<Rank> do
                    if (field >>> int rank) &&& 1 = 1 then
                        yield rank
            |]

        let fields = read ServerRecordType.Hand
        {
            Seat =
                fields.[1]
                    |> Int32.Parse
                    |> enum<Seat>
            Cards =
                let suits = 
                    [|
                        Suit.Spades, 2
                        Suit.Hearts, 3
                        Suit.Clubs, 4
                        Suit.Diamonds, 5
                    |]
                seq {
                    for (suit, iField) in suits do
                        let field = fields.[iField] |> Int32.Parse
                        for rank in readRanks field do
                            yield Card(rank, suit)
                } |> set
        }

    module private Card =

        /// Converts a KH card number to a card.
        let fromInt cardNum =
            let rank =
                (cardNum / Suit.numSuits) + 2 |> enum<Rank>
            let suit =
                match cardNum % Suit.numSuits with
                    | 0 -> Suit.Spades
                    | 1 -> Suit.Hearts
                    | 2 -> Suit.Clubs
                    | 3 -> Suit.Diamonds
                    | _ -> failwith "Unexpected"
            Card(rank, suit)

        /// Converts a card to a KH card number.
        let toInt (card : Card) =
            let rank =
                (int card.Rank - 2) * Suit.numSuits
            let suit =
                match card.Suit with
                    | Suit.Spades   -> 0
                    | Suit.Hearts   -> 1
                    | Suit.Clubs    -> 2
                    | Suit.Diamonds -> 3
                    | _ -> failwith "Unexpected"
            rank + suit

    /// Reads an outgoing or incoming exchange.
    let private readExchange recordType =
        let fields = read recordType
        {
            Seat =
                fields.[1]
                    |> Int32.Parse
                    |> enum<Seat>
            Cards =
                let cardNums =
                    fields.[3..5]
                        |> Array.map Int32.Parse
                if cardNums.[0] = -1 then
                    assert(cardNums |> Array.forall ((=) -1))
                    Set.empty
                else
                    cardNums
                        |> Seq.map Card.fromInt
                        |> set
        }

    /// Reads an outgoing exchange.
    let readExchangeOutgoing () =
        readExchange ServerRecordType.ExchangeOutgoing

    /// Reads an incoming exchange.
    let readExchangeIncoming () =
        readExchange ServerRecordType.ExchangeIncoming

    /// Reads a trick start record.
    let readTrickStart () =
        let fields = read ServerRecordType.TrickStart
        {
            Leader =
                fields.[1]
                    |> Int32.Parse
                    |> enum<Seat>
            TrickNum =
                fields.[2]
                    |> Int32.Parse
        }

    /// Reads a card played on a trick. Result might be empty.
    let readPlay () =
        let fields = read ServerRecordType.Play
        {
            Seat =
                fields.[1]
                    |> Int32.Parse
                    |> enum<Seat>
            Cards =
                let cardNum =
                    fields.[2]
                        |> Int32.Parse
                if cardNum = -1 then Set.empty
                else cardNum |> Card.fromInt |> Set.singleton
        }

    /// Writes the given fields as a message to KH.
    let private write (fields : string[]) =
        printfn $"write: |{String.Join(',', fields)}|"
        let chunks =
            [|
                yield "CHs"
                yield! fields
                yield "CHe"
            |]
        let str = String.Join(',', chunks)
        let buffer = Encoding.Default.GetBytes(str)
        sharedFile.Seek(0L, SeekOrigin.Begin) |> ignore
        sharedFile.Write(buffer, 0, buffer.Length)
        sharedFile.Flush()

    /// Writes an empty message of the given type.
    let writeEmpty (recordType : ClientRecordType) =
        [|
            $"{int recordType}    "
            "-1     "
            "-1     "
            "-1     "
            "-1     "
            "-1     "
        |] |> write

    /// Writes an outgoing exchange containing the given cards.
    let writeExchangeOutgoing cards =
        let cards = cards |> Seq.toArray
        [|
            sprintf "%-7d" <| int ClientRecordType.ExchangeOutgoing
            sprintf "%-7d" <| Card.toInt cards.[0]
            sprintf "%-7d" <| Card.toInt cards.[1]
            sprintf "%-7d" <| Card.toInt cards.[2]
            "-1     "
            "-1     "
        |] |> write

    /// Writes a card played on a trick.
    let writePlay card =
        [|
            sprintf "%-7d" <| int ClientRecordType.Play
            sprintf "%-7d" <| Card.toInt card
            "-1     "
            "-1     "
            "-1     "
            "-1     "
        |] |> write
