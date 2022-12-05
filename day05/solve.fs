module aoc.day05.solutions

open aoc.common

let parse (line: string) =
    let v =
        line
            .Replace("move ", "")
            .Replace("from ", "")
            .Replace("to ", "")
            .Split(' ')

    [| int v[0]; int v[1]; int v[2] |]

let move (acc: list<char> []) (l: int []) =
    let pos1 = l[1] - 1
    let pos2 = l[2] - 1
    let steps = l[0]

    for x in 1..steps do
        acc[pos2] <- (List.head acc[pos1]) :: acc[pos2]
        acc[pos1] <- List.tail acc[pos1]

    acc

let move2 (acc: list<char> []) (l: int []) =
    let pos1 = l[1] - 1
    let pos2 = l[2] - 1
    let steps = l[0]

    let mutable aux = []

    for x in 1..steps do
        aux <- List.head acc[pos1] :: aux
        acc[pos1] <- List.tail acc[pos1]

    for x in 1..steps do
        acc[pos2] <- List.head aux :: acc[pos2]
        aux <- List.tail aux

    acc

let solve1 inputStacks (lines: string []) =
    let stacks =
        inputStacks
        |> Array.map Seq.toList
        |> Array.map Seq.rev
        |> Array.map Seq.toList

    let finalStacks = lines |> Array.map parse |> Array.fold move stacks

    let ret =
        finalStacks
        |> Array.map List.head
        |> System.String

    ret

let solve2 inputStacks (lines: string []) =
    let stacks =
        inputStacks
        |> Array.map Seq.toList
        |> Array.map Seq.rev
        |> Array.map Seq.toList

    let finalStacks =
        lines
        |> Array.map parse
        |> Array.fold move2 stacks

    let ret =
        finalStacks
        |> Array.map List.head
        |> System.String

    ret
