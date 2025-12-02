namespace Hearts.TrainKiller

open System
open System.Text

open Hearts.ParseKiller

module Program =

    Console.OutputEncoding <- Encoding.Unicode
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let entries =
        Json.loadEntries @"C:\Users\brian\OneDrive\Desktop\KHearts.zero.json"
    stopwatch.Stop()
    printfn $"{entries.Length} entries in {stopwatch.Elapsed}"
