namespace Hearts.PlayKiller

open System
open System.IO
open System.Text
open System.Threading

open PlayingCards
open Hearts

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

type ClientRecordType =
    | SessionStart = 101
    | GameStart = 102
    | DealStart = 103
    | Hand = 104
    | ExchangeOutgoing = 105

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

    let readSessionStart () =
        let fields = read ()
        if Int32.Parse(fields.[0]) <> 1 then
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
        if Int32.Parse(fields.[0]) <> 2 then
            failwith "Incorrect key"
        {
            Dealer =
                fields.[1]
                    |> Int32.Parse
                    |> enum<Seat>
            DealNum = fields.[2] |> Int32.Parse
            NumGames = fields.[4] |> Int32.Parse
        }

    let readNewDeal () =
        let fields = read ()
        if Int32.Parse(fields.[0]) <> 3 then
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
        if Int32.Parse(fields.[0]) <> 4 then
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

    (*
    let readExchangeOutgoing clientSeats =
        let fields = read ()
        if Int32.Parse(fields.[0]) <> 5 then
            failwith "Incorrect key"
        let seat =
            fields.[1]
                |> Int32.Parse
                |> enum<Seat>
        if clientSeats |> Set.contains seat then
            assert(
                fields.[3..5]
                    |> Array.forall (fun field ->
                        Int32.Parse(field) = -1))
            None
        else
            Some {
                Seat = seat
                Cards =
                    fields.[3..5]
                        |> Seq.map (fun field ->
                            field |> Int32.Parse |> Card.fromInt)
                        |> set
            }
    *)

    let readPlay isClient =
        let fields = read ()
        if Int32.Parse(fields.[0]) <> 8 then
            failwith "Incorrect key"
        let seat =
            fields.[1]
                |> Int32.Parse
                |> enum<Seat>
        if isClient then
            assert(
                fields.[3..5]
                    |> Array.forall (fun field ->
                        Int32.Parse(field) = -1))
            None
        else
            Some {
                Seat = seat
                Cards =
                    fields.[3..5]
                        |> Seq.map (fun field ->
                            field |> Int32.Parse |> Card.fromInt)
                        |> set
            }

    let private write clientRecord =
        let chunks =
            [|
                "CHs"
                sprintf $"    {int clientRecord.RecordType}"
                "0000000"
                "0000000"
                "0000000"
                "0000000"
                "0000000"
                "CHe"
            |]
        let str = String.Join(',', chunks)
        let buffer = Encoding.Default.GetBytes(str)
        sharedFile.Seek(0L, SeekOrigin.Begin) |> ignore
        sharedFile.Write(buffer, 0, buffer.Length)
        sharedFile.Flush()

    let writeGeneral recordType =
        write { RecordType = recordType }
