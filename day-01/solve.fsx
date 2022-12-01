open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let lines = File.ReadAllLines("sample.txt") |> Array.toSeq

let splitByCond xs pred = 
    [
        let mutable lst=[]
        for line in xs do
            match line with
            | line when pred(line)=true ->
                yield lst
                lst<-[]
            | _->
                lst<-lst@[line]
        yield lst
    ]

let convertToInt = List.map (List.map (fun (x:string)->int x))

let caloriesInput = splitByCond lines (fun line->line="")

let calories = convertToInt caloriesInput

let totals = calories|>List.map List.sum
let max = List.max(totals)//74198
printfn $"{max}" 

let totals_sorted = totals|>List.sortDescending
let max3 = totals_sorted|>List.take(3)|>List.sum//209914
printfn $"{max3}" 

