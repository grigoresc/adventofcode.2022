open System
open System.IO
open System.Collections

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let lines = File.ReadAllLines("input.txt") |> Array.toSeq


let split (xs:seq<string>) =
    let rec loop (s:list<string>) (acc:list<int>) = [
        match s.IsEmpty with
        | false->
            match Seq.head s with
            | "" ->
                //printf $"acc={acc};"
                yield acc 
                yield! loop s.Tail [] 
            | _ ->
                //printf $"{s.Head}"
                yield! loop s.Tail (acc @ [int(s.Head)])
        | true -> yield acc 
        ]
    loop (Seq.toList(xs)) []

let x = split lines

let totals = x|>List.map List.sum 
let max = List.max(totals)//74198
printfn $"{max}" 

let totals_sorted = totals|>List.sortDescending
let max3 = totals_sorted|>List.take(3)|>List.sum//209914
printfn $"{max3}" 

