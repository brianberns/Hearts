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
    | TrickEnd = 9

type ServerSessionStartRecord =
    {
        ClientSeats : Set<Seat>
        HostDisplay : bool
        TwoOfClubsLeads : bool
    }

type ServerGameStartRecord =
    {
        Dealer : Seat
        DealNum : int
        NumGames : int
    }

type ServerDealStartRecord =
    {
        GameScore : Score
        ExchangeDirection : ExchangeDirection
    }

type ServerCardsRecord =
    {
        Seat : Seat
        Cards : Set<Card>
    }

type ServerTrickStartRecord =
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
    | TrickEnd = 109

type ClientRecord =
    {
        RecordType : ClientRecordType
    }

module SharedRecord =

    let sharedFile =
        File.Open(
            "C:\Program Files\KHearts\KH_Host_Client.dat",
            FileMode.Open,
            FileAccess.ReadWrite,
            FileShare.ReadWrite)

    let buffer = Array.zeroCreate<byte> 55

    let private read () =
        let rec loop sleep =
            Thread.Sleep(sleep : int)
            sharedFile.Seek(0L, SeekOrigin.Begin) |> ignore
            let nBytes = sharedFile.Read(buffer, 0, buffer.Length)
            assert(nBytes = buffer.Length)
            let str = Encoding.Default.GetString(buffer)
            let chunks = str.Split(',')
            if chunks.[0] = "HCs" && chunks.[7] = "HCe" then
                chunks.[1..6]
            elif sleep > 1000 then
                failwithf $"Incorrect header/trailer: {chunks.[0]}/{chunks.[7]}"
            else loop (2 * sleep)
        loop 1

    let readGeneral (recordType : ServerRecordType) =
        let fields = read ()
        if Int32.Parse(fields.[0]) <> int recordType then
            failwith "Incorrect key"
        ()

    let readSessionStart () =
        let fields = read ()
        if Int32.Parse(fields.[0]) <> int ServerRecordType.SessionStart then
            failwith "Incorrect key"
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

    let readGameStart () =
        let fields = read ()
        if Int32.Parse(fields.[0]) <> int ServerRecordType.GameStart then
            failwith "Incorrect key"
        {
            Dealer =
                fields.[1]
                    |> Int32.Parse
                    |> enum<Seat>
            DealNum = fields.[2] |> Int32.Parse
            NumGames = fields.[4] |> Int32.Parse
        }

    let readDealStart () =
        let fields = read ()
        if Int32.Parse(fields.[0]) <> int ServerRecordType.DealStart then
            failwith "Incorrect key"
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

    let readHand () =

        let readRanks (field : int) =
            [|
                for rank in Enum.getValues<Rank> do
                    if (field >>> int rank) &&& 1 = 1 then
                        yield rank
            |]

        let fields = read ()
        if Int32.Parse(fields.[0]) <> int ServerRecordType.Hand then
            failwith "Incorrect key"
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

    module Card =

        let fromInt value =
            let rank =
                (value / Suit.numSuits) + 2 |> enum<Rank>
            let suit =
                match value % Suit.numSuits with
                    | 0 -> Suit.Spades
                    | 1 -> Suit.Hearts
                    | 2 -> Suit.Clubs
                    | 3 -> Suit.Diamonds
                    | _ -> failwith "Unexpected"
            Card(rank, suit)

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

    let private readExchange (key : ServerRecordType)=
        let fields = read ()
        if Int32.Parse(fields.[0]) <> int key then
            failwith "Incorrect key"
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

    let readExchangeOutgoing () =
        readExchange ServerRecordType.ExchangeOutgoing

    let readExchangeIncoming () =
        readExchange ServerRecordType.ExchangeIncoming

    let readTrickStart () =
        let fields = read ()
        if Int32.Parse(fields.[0]) <> int ServerRecordType.TrickStart then
            failwith "Incorrect key"
        {
            Leader =
                fields.[1]
                    |> Int32.Parse
                    |> enum<Seat>
            TrickNum =
                fields.[2]
                    |> Int32.Parse
        }

    let readPlay () =
        let fields = read ()
        if Int32.Parse(fields.[0]) <> int ServerRecordType.Play then
            failwith "Incorrect key"
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

    let private write clientRecord =
        let chunks =
            [|
                "CHs"
                $"{int clientRecord.RecordType}    "
                "-1     "
                "-1     "
                "-1     "
                "-1     "
                "-1     "
                "CHe"
            |]
        let str = String.Join(',', chunks)
        let buffer = Encoding.Default.GetBytes(str)
        sharedFile.Seek(0L, SeekOrigin.Begin) |> ignore
        sharedFile.Write(buffer, 0, buffer.Length)
        sharedFile.Flush()

    let writeGeneral recordType =
        write { RecordType = recordType }

    let writeExchangeOutgoing cards =
        let cards = cards |> Seq.toArray
        let chunks =
            [|
                "CHs"
                sprintf "%7d" <| 105
                sprintf "%7d" <| Card.toInt cards.[0]
                sprintf "%7d" <| Card.toInt cards.[1]
                sprintf "%7d" <| Card.toInt cards.[2]
                "-1     "
                "-1     "
                "CHe"
            |]
        let str = String.Join(',', chunks)
        let buffer = Encoding.Default.GetBytes(str)
        sharedFile.Seek(0L, SeekOrigin.Begin) |> ignore
        sharedFile.Write(buffer, 0, buffer.Length)
        sharedFile.Flush()

    let writePlay card =
        let chunks =
            [|
                "CHs"
                sprintf "%7d" <| 108
                sprintf "%7d" <| Card.toInt card
                "-1     "
                "-1     "
                "-1     "
                "-1     "
                "CHe"
            |]
        let str = String.Join(',', chunks)
        let buffer = Encoding.Default.GetBytes(str)
        sharedFile.Seek(0L, SeekOrigin.Begin) |> ignore
        sharedFile.Write(buffer, 0, buffer.Length)
        sharedFile.Flush()
