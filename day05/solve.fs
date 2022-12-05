module aoc.day05.solutions

open aoc.common

let parse (line: string) =
    let v = line.Replace("move ", "")
    let v = v.Replace("from ", "")
    let v = v.Replace("to ", "")
    let v = v.Split(' ')
    [| int v[0]; int v[1]; int v[2] |]

let move (acc: list<char> []) (l: int []) =
    let pos1 = l[1] - 1
    let pos2 = l[2] - 1
    let steps = l[0]

    let mutable a = acc

    let mutable ai = []

    for x in 1..steps do
        a[pos2] <- (List.head a[pos1]) :: a[pos2]
        a[pos1] <- List.tail a[pos1]

    acc

let move2 (acc: list<char> []) (l: int []) =
    let pos1 = l[1] - 1
    let pos2 = l[2] - 1
    let steps = l[0]

    let mutable a = acc

    let mutable ai = []

    for x in 1..steps do
        ai <- (List.head a[pos1]) :: ai
        a[pos1] <- List.tail a[pos1]

    for x in 1..steps do
        a[pos2] <- (List.head ai) :: a[pos2]
        ai <- List.tail ai

    acc

let solve1 x (lines: string []) =
    let lst =
        x
        |> Array.map Seq.toList
        |> Array.map Seq.rev
        |> Array.map Seq.toList

    let x = lines |> Array.map parse |> Array.fold move lst

    let ret = x |> Array.map List.head |> System.String
    ret

let solve2 x (lines: string []) =
    let lst =
        x
        |> Array.map Seq.toList
        |> Array.map Seq.rev
        |> Array.map Seq.toList

    let x = lines |> Array.map parse |> Array.fold move2 lst

    let ret = x |> Array.map List.head |> System.String
    ret
