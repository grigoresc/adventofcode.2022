module aoc.day02.solutions

open System
open System.IO
open aoc.common

let solve lines =

    let score (line: String) =
        let elf = line.[0]
        let me = line.[2]

        let elfIdx =
            match elf with
            | 'A' -> 1
            | 'B' -> 2
            | 'C' -> 3
            | _ -> failwith "elf"

        let meIdx =
            match me with
            | 'X' -> 1
            | 'Y' -> 2
            | 'Z' -> 3
            | _ -> failwith "me"

        let score1 = meIdx

        let score2 =
            match (elfIdx, meIdx) with
            | (1, 3) -> 0
            | (3, 1) -> 6
            | (elf, my) when elf < my -> 6
            | (elf, my) when elf = my -> 3
            | (elf, my) when elf > my -> 0
            | _ -> failwith "score2"

        score1 + score2

    let sln1 = lines |> Array.map score |> Array.sum //12156
    print sln1

    let score2 (line: String) =
        let elf = line.[0]
        let res = line.[2]

        let elfIdx =
            match elf with
            | 'A' -> 1
            | 'B' -> 2
            | 'C' -> 3
            | _ -> failwith "elf"

        let score1 =
            match res with
            | 'X' -> 0
            | 'Y' -> 3
            | 'Z' -> 6
            | _ -> failwith "score1"

        let score2 =
            match (elfIdx, res) with
            | (elf, 'Y') -> elf
            | (1, 'X') -> 3
            | (elf, 'X') -> elf - 1
            | (3, 'Z') -> 1
            | (elf, 'Z') -> elf + 1
            | _ -> failwith "score2"

        score1 + score2

    let sln2 = lines |> Array.map score2 |> Array.sum //10835
    print sln2
    (sln1, sln2)

//solve "sample.txt"
