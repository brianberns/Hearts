namespace Hearts.Cfr

open System.IO
open Microsoft.Data.Sqlite

open Hearts

module Cfr =

    let tryGetActionIndex dir infoSet =
        let path = Path.Combine(dir, "Hearts.db")
        use conn = new SqliteConnection($"Data Source={path}")
        conn.Open()

        do
            use pragmaCmd =
                conn.CreateCommand(
                    CommandText = "PRAGMA journal_mode = WAL;")   // performance optimization
            pragmaCmd.ExecuteNonQuery() |> ignore

        use selectCmd =
            conn.CreateCommand(
                CommandText =
                    "select Action from Strategy where Key = $Key")
        let key =
            infoSet
                |> Encoding.encode
                |> Encoding.toString
        selectCmd.Parameters.AddWithValue("$Key", key)
            |> ignore

        let actionObj = selectCmd.ExecuteScalar()
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

    let player dir = { Act = act dir }
