namespace Hearts.Learn

open System
open Microsoft.Data.Sqlite

/// Checkpoint record from database.
type CheckpointRecord =
    {
        Id : int
        Iteration : int
        NumSeen : int64
        Count : int
        BaseSeed : int
        ModelPath : string
        CreatedAt : DateTime
    }

/// Training history statistics.
type HistoryStats =
    {
        NumSamples : int
        ReservoirSize : int
        TrainingDurationMs : int64 option
        TournamentPayoff : float option
    }

/// SQLite checkpoint and history management.
module Checkpoint =

    /// Creates the database tables if they don't exist.
    let private ensureTables (conn : SqliteConnection) =
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            CREATE TABLE IF NOT EXISTS Checkpoint (
                Id INTEGER PRIMARY KEY,
                Iteration INTEGER NOT NULL,
                NumSeen INTEGER NOT NULL,
                Count INTEGER NOT NULL,
                BaseSeed INTEGER NOT NULL,
                ModelPath TEXT NOT NULL,
                CreatedAt TEXT NOT NULL
            );

            CREATE TABLE IF NOT EXISTS TrainingHistory (
                Iteration INTEGER PRIMARY KEY,
                NumSamples INTEGER NOT NULL,
                ReservoirSize INTEGER NOT NULL,
                TrainingDurationMs INTEGER,
                TournamentPayoff REAL,
                CreatedAt TEXT NOT NULL
            );
        """
        cmd.ExecuteNonQuery() |> ignore

    /// Opens a connection to the database.
    let private openConnection dbPath =
        let conn = new SqliteConnection($"Data Source={dbPath}")
        conn.Open()
        ensureTables conn
        conn

    /// Saves a checkpoint to the database.
    let save dbPath iteration numSeen count baseSeed modelPath =
        use conn = openConnection dbPath
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            INSERT INTO Checkpoint (Iteration, NumSeen, Count, BaseSeed, ModelPath, CreatedAt)
            VALUES (@iteration, @numSeen, @count, @baseSeed, @modelPath, @createdAt)
        """
        cmd.Parameters.AddWithValue("@iteration", iteration) |> ignore
        cmd.Parameters.AddWithValue("@numSeen", numSeen) |> ignore
        cmd.Parameters.AddWithValue("@count", count) |> ignore
        cmd.Parameters.AddWithValue("@baseSeed", baseSeed) |> ignore
        cmd.Parameters.AddWithValue("@modelPath", modelPath) |> ignore
        cmd.Parameters.AddWithValue("@createdAt", DateTime.UtcNow.ToString("o")) |> ignore
        cmd.ExecuteNonQuery() |> ignore

    /// Tries to load the latest checkpoint from the database.
    let tryLoadLatest dbPath =
        if not (IO.File.Exists dbPath) then
            None
        else
            use conn = openConnection dbPath
            use cmd = conn.CreateCommand()
            cmd.CommandText <- """
                SELECT Id, Iteration, NumSeen, Count, BaseSeed, ModelPath, CreatedAt
                FROM Checkpoint
                ORDER BY Iteration DESC
                LIMIT 1
            """
            use reader = cmd.ExecuteReader()
            if reader.Read() then
                Some {
                    Id = reader.GetInt32(0)
                    Iteration = reader.GetInt32(1)
                    NumSeen = reader.GetInt64(2)
                    Count = reader.GetInt32(3)
                    BaseSeed = reader.GetInt32(4)
                    ModelPath = reader.GetString(5)
                    CreatedAt = DateTime.Parse(reader.GetString(6))
                }
            else
                None

    /// Records training history for an iteration.
    let recordHistory dbPath iteration (stats : HistoryStats) =
        use conn = openConnection dbPath
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            INSERT OR REPLACE INTO TrainingHistory
            (Iteration, NumSamples, ReservoirSize, TrainingDurationMs, TournamentPayoff, CreatedAt)
            VALUES (@iteration, @numSamples, @reservoirSize, @durationMs, @payoff, @createdAt)
        """
        cmd.Parameters.AddWithValue("@iteration", iteration) |> ignore
        cmd.Parameters.AddWithValue("@numSamples", stats.NumSamples) |> ignore
        cmd.Parameters.AddWithValue("@reservoirSize", stats.ReservoirSize) |> ignore
        cmd.Parameters.AddWithValue("@durationMs",
            stats.TrainingDurationMs
                |> Option.map box
                |> Option.defaultValue (box DBNull.Value)) |> ignore
        cmd.Parameters.AddWithValue("@payoff",
            stats.TournamentPayoff
                |> Option.map box
                |> Option.defaultValue (box DBNull.Value)) |> ignore
        cmd.Parameters.AddWithValue("@createdAt", DateTime.UtcNow.ToString("o")) |> ignore
        cmd.ExecuteNonQuery() |> ignore
