namespace Hearts.ParseKiller

module Program =

    let write () =
        Log.convertToJson
            @"C:\Users\brian\OneDrive\Desktop\KHearts.log"
            "KHearts.json"

    let read () =
        Json.loadEntries "KHearts.json"
            |> printfn "%A'"

    write ()
