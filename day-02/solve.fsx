open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#load "../common.fsx"

open Common

//let convertToInt = List.map (List.map (fun (x: string) -> int x))

let lines = File.ReadAllLines("input.txt")

let score (line: String) =
    let elf = line.[0]
    let me = line.[2]

    let elfScore =
        match elf with
        | 'A' -> 1
        | 'B' -> 2
        | 'C' -> 3
        | _ -> raise (Exception("elf"))

    let meScore =
        match me with
        | 'X' -> 1
        | 'Y' -> 2
        | 'Z' -> 3
        | _ -> raise (Exception("me"))

    let score1 = meScore

    let score2 =
        match (elfScore, meScore) with
        | (1, 3) -> 0
        | (3, 1) -> 6
        | (mes, elfs) when mes < elfs -> 6
        | (mes, elfs) when mes = elfs -> 3
        | (mes, elfs) when mes > elfs -> 0
        | _ -> raise (Exception("game"))

    //print $"{elf} vs {me} is {score1} and {score2}"
    score1 + score2

let sln1 = lines |> Array.map score |> Array.sum //12156
print sln1

let score2 (line: String) =
    let elf = line.[0]
    let res = line.[2]
    //print me
    //print elf

    let elfScore =
        match elf with
        | 'A' -> 1
        | 'B' -> 2
        | 'C' -> 3
        | _ -> raise (Exception("elf"))

    let resScore =
        match res with
        | 'X' -> 0
        | 'Y' -> 3
        | 'Z' -> 6
        | _ -> raise (Exception("res"))

    let score1 = resScore

    let score2 =
        match (elfScore, res) with
        | (elfs, 'Y') -> elfs
        | (1, 'X') -> 3
        | (elfs, 'X') -> elfs - 1
        | (3, 'Z') -> 1
        | (elfs, 'Z') -> elfs + 1
        | _ -> raise (Exception("game"))

    //print $"{elf} should {res} is {score1} and {score2}"
    score1 + score2

let sln2 = lines |> Array.map score2 |> Array.sum //10835
print sln2
