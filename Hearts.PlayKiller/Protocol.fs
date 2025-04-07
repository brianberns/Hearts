namespace Hearts.PlayKiller

open System
open System.IO
open System.Text
open System.Threading

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
                let field = fields.[0] |> Int32.Parse
                Enum.getValues<Seat>
                    |> Seq.where (fun seat ->
                        (field >>> int seat) &&& 1 = 1)
                    |> set
            HostDisplay =
                Int32.Parse(fields.[3]) = 1
            TwoOfClubsLeads =
                Int32.Parse(fields.[4]) = 0
        }

    /// Reads a game start record.
    let private readGameStart (fields : _[]) =
        GameStart {
            Dealer =
                fields.[0]
                    |> Int32.Parse
                    |> enum<Seat>
            DealNum = fields.[1] |> Int32.Parse
            NumGames = fields.[3] |> Int32.Parse
        }

    /// Parses a score from the given fields.
    let private parseScore (fields : _[]) =
        let scoreMap =
            Enum.getValues<Seat>
                |> Seq.map (fun seat ->
                    let points =
                        fields.[int seat] |> Int32.Parse
                    seat, points)
                |> Map
        { ScoreMap = scoreMap }

    /// Reads a deal start record.
    let readDealStart fields =
        DealStart {
            GameScore = parseScore fields
            ExchangeDirection =
                match fields.[4] |> Int32.Parse with
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
            Seat =
                fields.[0]
                    |> Int32.Parse
                    |> enum<Seat>
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
    let private readExchange (fields : _[]) =
        {
            Seat =
                fields.[0]
                    |> Int32.Parse
                    |> enum<Seat>
            Cards =
                let cardNums =
                    fields.[2..4]
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
    let private readExchangeOutgoing fields =
        fields |> readExchange |> ExchangeOutgoing

    /// Reads an incoming exchange.
    let private readExchangeIncoming fields =
        fields |> readExchange |> ExchangeIncoming

    /// Reads a trick start record.
    let private readTrickStart (fields : _[]) =
        TrickStart {
            Leader =
                fields.[0]
                    |> Int32.Parse
                    |> enum<Seat>
            TrickNum =
                fields.[1]
                    |> Int32.Parse
        }

    /// Reads a card played on a trick. Result might be empty.
    let private readPlay (fields : _[]) =
        Play {
            Seat =
                fields.[0]
                    |> Int32.Parse
                    |> enum<Seat>
            Cards =
                let cardNum =
                    fields.[1]
                        |> Int32.Parse
                if cardNum = -1 then Set.empty
                else cardNum |> Card.fromInt |> Set.singleton
        }

    /// Reads a trick finish record.
    let private readTrickFinish (_ : string[]) =
        TrickFinish

    /// Reads a deal finish record.
    let private readDealFinish (_ : string[]) =
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

        let rec loop sleep =
            if sleep >= 0 then
                Thread.Sleep(sleep : int)
            sharedFile.Seek(0L, SeekOrigin.Begin) |> ignore
            let nBytes = sharedFile.Read(buffer, 0, buffer.Length)
            assert(nBytes = buffer.Length)
            let str = Encoding.Default.GetString(buffer)
            let chunks = str.Split(',')
            if chunks.[0] = "HCs" && chunks.[7] = "HCe" then
#if DEBUG
                printfn $"read:  |{String.Join(',', chunks.[1..6])}|"
#endif
                chunks.[1..6]
            else
                if sleep > 1000 then sleep
                elif sleep > 0 then 2 * sleep
                else sleep + 1
                |> loop

        let fields = loop -3
        let reader =
            match fields.[0] |> Int32.Parse |> enum<ServerRecordType> with
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

    /// Writes the given fields as a message to KH.
    let private write (fields : string[]) =
#if DEBUG
        printfn $"write: |{String.Join(',', fields)}|"
#endif
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
