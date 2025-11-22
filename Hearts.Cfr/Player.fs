namespace Hearts.Cfr

open System.IO
open Microsoft.Data.Sqlite

open Hearts

module Cfr =

    let private openConnection dir =
        let path = Path.Combine(dir, "Hearts.db")
        let conn = new SqliteConnection($"Data Source={path}")
        conn.Open()
        conn

    let private optimize dir =
        use conn = openConnection dir
        use cmd =
            conn.CreateCommand(
                CommandText = "PRAGMA journal_mode = WAL;")
        cmd.ExecuteNonQuery() |> ignore

    let tryGetActionIndex dir infoSet =
        use conn = openConnection dir

        use cmd =
            conn.CreateCommand(
                CommandText =
                    "select Action from Strategy where Key = $Key")
        let key =
            infoSet
                |> Encoding.encode
                |> Encoding.toString
        cmd.Parameters.AddWithValue("$Key", key)
            |> ignore

        let actionObj = cmd.ExecuteScalar()
        if isNull actionObj then None
        else
            let actionIdx = actionObj :?> int64 |> int
            assert(actionIdx >= 0
                && actionIdx < infoSet.LegalActions.Length)
            Some actionIdx

    let getActionIndex dir infoSet =
        infoSet
            |> tryGetActionIndex dir
            |> Option.defaultValue 0

    let private act dir infoSet =
        let iAction = getActionIndex dir infoSet
        infoSet.LegalActions[iAction]

    let player dir =
        optimize dir   // one-time performance optimization
        { Act = act dir }
