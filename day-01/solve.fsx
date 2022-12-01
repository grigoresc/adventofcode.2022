open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let splitByCond predicate xs = 
    [
        let mutable lst=[]
        for line in xs do
            match line with
            | line when predicate(line)=true ->
                yield lst
                lst<-[]
            | _->
                lst<-lst@[line]
        yield lst
    ]

let convertToInt = List.map (List.map (fun (x:string)->int x))

let read inputLines = 
    inputLines
    |>splitByCond (fun line->line="")
    |>convertToInt


let lines = File.ReadAllLines("input.txt") 
let calories = read lines 

let totals = calories|>List.map List.sum
let max = List.max(totals)//74198
printfn $"{max}" 

let totals_sorted = totals|>List.sortDescending
let max3 = totals_sorted|>List.take(3)|>List.sum//209914
printfn $"{max3}" 

