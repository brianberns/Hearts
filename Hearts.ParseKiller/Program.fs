namespace Hearts.ParseKiller

open System
open System.Text

module Program =

    let write () =
        Log.convertToJson
            @"C:\Users\brian\OneDrive\Desktop\KHearts.log"
            "KHearts.json"

    let read () =
        Json.loadEntries "KHearts.json"
            |> Json.saveEntries "KHearts.copy.json"

    Console.OutputEncoding <- Encoding.Unicode
    read ()
