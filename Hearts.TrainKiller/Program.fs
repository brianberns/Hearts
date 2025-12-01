namespace Hearts.TrainKiller

open System
open System.Text

open Hearts
open Hearts.ParseKiller

module Program =
    Console.OutputEncoding <- Encoding.Unicode
    let entries =
        Json.loadEntries @"C:\Users\brian\OneDrive\Desktop\KHearts.json"
            |> Array.where (fun entry ->
                entry.GameScore = Score.zero)
    printfn "%A entries" entries.Length
