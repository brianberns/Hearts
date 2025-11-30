namespace Hearts.ParseKiller

open System
open System.Text

module Program =
    Console.OutputEncoding <- Encoding.Unicode
    Log.convertToJson
        @"C:\Users\brian\OneDrive\Desktop\KHearts.log"
        "KHearts.json"
    Json.loadEntries "KHearts.json"
        |> Json.saveEntries "KHearts.copy.json"
