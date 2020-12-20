namespace Hearts.PlayKiller

open System
open System.IO
open System.Text

open PlayingCards
open Hearts

type InitRecord =
    {
        ClientSeats : Set<Seat>
        HostDisplay : bool
        TwoOfClubsLeads : bool
    }

module SharedRecord =

    let sharedFile =
        File.Open(
            "C:\Program Files\KHearts\KH_Host_Client.dat",
            FileMode.Open,
            FileAccess.ReadWrite,
            FileShare.ReadWrite)

    let buffer =
        Array.zeroCreate<byte> 55

    let private read () =
        sharedFile.Seek(0L, SeekOrigin.Begin) |> ignore
        let nBytes = sharedFile.Read(buffer, 0, buffer.Length)
        assert(nBytes = buffer.Length)
        let str = Encoding.Default.GetString(buffer)
        let chunks = str.Split(',')
        if chunks.[0] <> "HCs" then
            failwith "Incorrect header"
        if chunks.[7] <> "HCe" then
            failwith "Incorrect trailer"
        chunks.[1..6]

    let readInit () =
        let fields = read ()
        if Int32.Parse(fields.[0]) <> 1 then
            failwith "Incorrect key"
        {
            ClientSeats =
                let bitmap = Int32.Parse(fields.[1])
                Seat.allSeats
                    |> Seq.where (fun seat ->
                        (1 <<< int seat) &&& bitmap <> 0)
                    |> set
            HostDisplay =
                Int32.Parse(fields.[4]) = 1
            TwoOfClubsLeads =
                Int32.Parse(fields.[5]) = 0
        }

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
        printfn "%A" <| record
        assert(record.ClientSeats |> Seq.exactlyOne = Seat.South)
        assert(record.TwoOfClubsLeads)

    [<EntryPoint>]
    let main argv =
        try
            run ()
        with ex ->
            printfn "%s" ex.Message
        0
