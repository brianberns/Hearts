namespace Hearts.PlayKiller

open System
open System.IO
open System.Text
open System.Threading

open PlayingCards
open Hearts

type ServerInitRecord =
    {
        ClientSeats : Set<Seat>
        HostDisplay : bool
        TwoOfClubsLeads : bool
    }

type ServerNewGameRecord =
    {
        Dealer : Seat
        DealNum : int
        NumGames : int
    }

type ServerNewDealRecord =
    {
        Score : Score
        ExchangeDirection : ExchangeDirection
    }

type ServerHandRecord =
    {
        Seat : Seat
        Hand : Card[]
    }

type ClientRecordType =
    | Initialization = 101
    | NewGame = 102
    | NewDeal = 103
    | Hand = 104

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

    let readInit () =
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

    let readNewGame () =
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
            Score =
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
            Hand =
                let suits = 
                    [|
                        Suit.Spades, 2
                        Suit.Hearts, 3
                        Suit.Clubs, 4
                        Suit.Diamonds, 5
                    |]
                [|
                    for (suit, iField) in suits do
                        let field = fields.[iField] |> Int32.Parse
                        for rank in readRanks field do
                            yield Card(rank, suit)
                |]
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

module Program =

    let rng = Random(0)

    let currentHand deal =
        let seat =
            deal.ClosedDeal
                |> ClosedDeal.currentPlayer
        deal.UnplayedCardMap.[seat]

    let makePass deal _ =
        deal
            |> currentHand
            |> Seq.toArray
            |> Array.shuffle rng
            |> Seq.take Exchange.numCards
            |> set

    let makePlay deal _ =
        let hand = currentHand deal
        deal.ClosedDeal
            |> ClosedDeal.legalPlays hand
            |> Seq.toArray
            |> Array.shuffle rng
            |> Seq.head

    let randomPlayer =
        {
            MakePass = makePass
            MakePlay = makePlay
        }

    let run () =

        let record = SharedRecord.readInit ()
        printfn "%A" record
        assert(record.ClientSeats = set [| Seat.East; Seat.West |])
        assert(record.TwoOfClubsLeads)
        SharedRecord.writeGeneral ClientRecordType.Initialization

        let record = SharedRecord.readNewGame ()
        printfn "%A" record
        SharedRecord.writeGeneral ClientRecordType.NewGame

        let record = SharedRecord.readNewDeal ()
        printfn "%A" record
        SharedRecord.writeGeneral ClientRecordType.NewDeal

        for i = 0 to 3 do
            let record = SharedRecord.readHand ()
            printfn "%A" record
            assert(record.Hand.Length = ClosedDeal.numCardsPerHand)
            SharedRecord.writeGeneral ClientRecordType.Hand

    [<EntryPoint>]
    let main argv =
        try
            run ()
        with ex ->
            printfn "%s" ex.Message
        0
