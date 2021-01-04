namespace Hearts.PlayKiller

open System
open System.IO
open System.Text

open PlayingCards
open Hearts

/// Record types sent from server to client.
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
    | GameFinish = 11
    | SessionFinish = 12

/// Session start record.
type SessionStartRecord =
    {
        ClientSeats : Set<Seat>
        HostDisplay : bool
        TwoOfClubsLeads : bool
    }

/// Game start record.
type GameStartRecord =
    {
        Dealer : Seat
        DealNum : int
        NumGames : int
    }

/// Deal start record.
type DealStartRecord =
    {
        GameScore : Score
        ExchangeDirection : ExchangeDirection
    }

/// Seat/cards record.
type SeatCardsRecord =
    {
        Seat : Seat
        Cards : Set<Card>
    }

/// Trick start record.
type TrickStartRecord =
    {
        Leader : Seat
        TrickNum : int
    }

/// Game finish record.
type GameFinishRecord =
    {
        GameScore : Score
    }

/// Session finish record.
type SessionFinishRecord =
    {
        SessionScore : Score
    }

/// Record sent from server to client.
type ServerRecord =
    | SessionStart of SessionStartRecord
    | GameStart of GameStartRecord
    | DealStart of DealStartRecord
    | Hand of SeatCardsRecord
    | ExchangeOutgoing of SeatCardsRecord
    | ExchangeIncoming of SeatCardsRecord
    | TrickStart of TrickStartRecord
    | Play of SeatCardsRecord
    | TrickFinish
    | DealFinish
    | GameFinish of GameFinishRecord
    | SessionFinish of SessionFinishRecord

/// Record type sent from client to server.
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
    | GameFinish = 111

/// Low-level protocol for interacting with Killer Hearts.
module Protocol =

    /// File used to send messages in both directions.
    let sharedFile =
        File.Open(
            "C:\Program Files\KHearts\KH_Host_Client.dat",
            FileMode.Open,
            FileAccess.ReadWrite,
            FileShare.ReadWrite)

    /// Reads a session start record.
    let private readSessionStart (fields : _[]) =
        SessionStart {
            ClientSeats =
                let field = fields.[0]
                Seat.allSeats
                    |> Seq.where (fun seat ->
                        (field >>> int seat) &&& 1 = 1)
                    |> set
            HostDisplay =
                fields.[3] = 1
            TwoOfClubsLeads =
                fields.[4] = 0
        }

    /// Reads a game start record.
    let private readGameStart (fields : _[]) =
        GameStart {
            Dealer = fields.[0] |> enum<Seat>
            DealNum = fields.[1]
            NumGames = fields.[3]
        }

    /// Parses a score from the given fields.
    let private parseScore (fields : _[]) =
        Seat.allSeats
            |> Seq.map (fun seat ->
                let points = fields.[int seat]
                seat, points)
            |> Map
            |> ScoreMap

    /// Reads a deal start record.
    let readDealStart fields =
        DealStart {
            GameScore = parseScore fields
            ExchangeDirection =
                match fields.[4] with
                    | 0 -> ExchangeDirection.Left
                    | 1 -> ExchangeDirection.Right
                    | 2 -> ExchangeDirection.Across
                    | 3 -> ExchangeDirection.Hold
                    | _ -> failwith "Unexpected"
        }

    /// Reads a hand.
    let private readHand (fields : _[]) =

        let readRanks (field : int) =
            [|
                for rank in Enum.getValues<Rank> do
                    if (field >>> int rank) &&& 1 = 1 then
                        yield rank
            |]

        Hand {
            Seat = fields.[0] |> enum<Seat>
            Cards =
                let suits = 
                    [|
                        Suit.Spades, 1
                        Suit.Hearts, 2
                        Suit.Clubs, 3
                        Suit.Diamonds, 4
                    |]
                seq {
                    for (suit, iField) in suits do
                        let field = fields.[iField]
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
    let private readExchange (fields : _[]) =
        {
            Seat = fields.[0] |> enum<Seat>
            Cards =
                let cardNums = fields.[2..4]
                if cardNums.[0] = -1 then
                    assert(cardNums |> Array.forall ((=) -1))
                    Set.empty
                else
                    cardNums
                        |> Seq.map Card.fromInt
                        |> set
        }

    /// Reads an outgoing exchange.
    let private readExchangeOutgoing fields =
        fields |> readExchange |> ExchangeOutgoing

    /// Reads an incoming exchange.
    let private readExchangeIncoming fields =
        fields |> readExchange |> ExchangeIncoming

    /// Reads a trick start record.
    let private readTrickStart (fields : _[]) =
        TrickStart {
            Leader = fields.[0] |> enum<Seat>
            TrickNum = fields.[1]
        }

    /// Reads a card played on a trick. Result might be empty.
    let private readPlay (fields : _[]) =
        Play {
            Seat = fields.[0] |> enum<Seat>
            Cards =
                let cardNum = fields.[1]
                if cardNum = -1 then Set.empty
                else cardNum |> Card.fromInt |> Set.singleton
        }

    /// Reads a trick finish record.
    let private readTrickFinish (_ : int[]) =
        TrickFinish

    /// Reads a deal finish record.
    let private readDealFinish (_ : int[]) =
        DealFinish

    /// Reads a game finish record.
    let private readGameFinish (fields : _[]) =
        GameFinish {
            GameScore = parseScore fields
        }

    /// Reads a session finish record.
    let private readSessionFinish (fields : _[]) =
        SessionFinish {
            SessionScore = parseScore fields
        }

    /// Buffer used for reading messages.
    let buffer = Array.zeroCreate<byte> 55

    /// Reads a message from KH.
    let read () =

        let rec loop () =
            System.Threading.Thread.Sleep(0)
            sharedFile.Seek(0L, SeekOrigin.Begin) |> ignore
            let nBytes = sharedFile.Read(buffer, 0, buffer.Length)
            assert(nBytes = buffer.Length)
            let str = Encoding.Default.GetString(buffer)
            let chunks = str.Split(',')
            if chunks.[0] = "HCs" && chunks.[7] = "HCe" then
                // printfn $"read:  |{String.Join(',', chunks.[1..6])}|"
                let flagFields =
                    chunks.[1..6]
                        |> Array.map Int32.TryParse
                if flagFields |> Array.forall fst then
                    let nSigns =
                        flagFields.[3..]
                            |> Seq.map (fun (_, field) ->
                                field >= 0)
                            |> Seq.distinct
                            |> Seq.length
                    if nSigns = 1 then
                        flagFields |> Array.map snd
                    else loop ()
                else loop ()
            else loop ()

        let fields = loop ()
        let reader =
            match fields.[0] |> enum<ServerRecordType> with
                | ServerRecordType.SessionStart -> readSessionStart
                | ServerRecordType.GameStart -> readGameStart
                | ServerRecordType.DealStart -> readDealStart
                | ServerRecordType.Hand -> readHand
                | ServerRecordType.ExchangeOutgoing -> readExchangeOutgoing
                | ServerRecordType.ExchangeIncoming -> readExchangeIncoming
                | ServerRecordType.TrickStart -> readTrickStart
                | ServerRecordType.Play -> readPlay
                | ServerRecordType.TrickFinish -> readTrickFinish
                | ServerRecordType.DealFinish -> readDealFinish
                | ServerRecordType.GameFinish -> readGameFinish
                | ServerRecordType.SessionFinish -> readSessionFinish
                | _ -> failwith $"Unexpected server record type: {fields.[0]}"
        reader fields.[1..]

    /// Client -> Server header.
    let private headerBuffer =
        Encoding.Default.GetBytes("CHs,")

    /// Client -> Server footer.
    let private footerBuffer =
        Encoding.Default.GetBytes(",CHe")

    /// Writes the given fields as a message to KH.
    let private write (fields : string[]) =
        // printfn $"write: |{String.Join(',', fields)}|"

            // write message body first
        let str = String.Join(',', fields)
        let buffer = Encoding.Default.GetBytes(str)
        sharedFile.Seek(int64 headerBuffer.Length, SeekOrigin.Begin) |> ignore
        sharedFile.Write(buffer, 0, buffer.Length)
        sharedFile.Flush()

            // write header and footer last
        sharedFile.Seek(0L, SeekOrigin.Begin) |> ignore
        sharedFile.Write(headerBuffer, 0, headerBuffer.Length)
        sharedFile.Seek(int64 (-footerBuffer.Length), SeekOrigin.End) |> ignore
        sharedFile.Write(footerBuffer, 0, footerBuffer.Length)
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
