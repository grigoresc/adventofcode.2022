open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#load "../common.fsx"

open Common

let convertToInt = List.map (List.map (fun (x: string) -> int x))

let readCalories =
    splitByCond (fun line -> line = "")
    >> convertToInt

let calories = readCalories <| File.ReadAllLines("input.txt")

let totals = calories |> List.map List.sum
let max = List.max (totals) //74198
print max

let totals_sorted = totals |> List.sortDescending
let max3 = totals_sorted |> List.take (3) |> List.sum //209914
print max3
