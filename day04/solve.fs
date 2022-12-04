module aoc.day04.solutions

let parse (line: string) =
    let p = line.Split(',')
    let r1 = p[ 0 ].Split('-')
    let r2 = p[ 1 ].Split('-')
    (int r1[0], int r1[1]), (int r2[0], int r2[1])

let overlap (range1, range2) =
    match range1, range2 with
    | r1, r2 when fst (r1) <= fst (r2) && snd (r2) <= snd (r1) -> 1
    | r1, r2 when fst (r2) <= fst (r1) && snd (r1) <= snd (r2) -> 1
    | _ -> 0

let overlap2 (range1, range2) =
    match range1, range2 with
    | r1, r2 when snd (r1) < fst (r2) -> 0
    | r1, r2 when fst (r1) > snd (r2) -> 0
    | _ -> 1

let solve1 = Array.map parse >> Array.map overlap >> Array.sum
let solve2 = Array.map parse >> Array.map overlap2 >> Array.sum
