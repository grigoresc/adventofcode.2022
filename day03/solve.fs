module aoc.day03.solutions

let prio (c: char) =
    match c with
    | c when 'a' <= c && c <= 'z' -> (1 + int c - int 'a')
    | _ -> (1 + int c - int 'A' + 26)

let solve1 (lines: string []) =
    let common (line: string) =
        let len = line.Length
        let p1 = line[0 .. len / 2 - 1]
        let p2 = line[len / 2 .. len - 1]

        let common =
            set (p1)
            |> Set.intersect (set (p2))
            |> Set.toArray

        common[0]

    lines
    |> Array.map common
    |> Array.map prio
    |> Array.sum

let solve2 (lines: string []) =
    let common (line: string []) =
        let common =
            set (line[0])
            |> Set.intersect (set (line[1]))
            |> Set.intersect (set (line[2]))
            |> Set.toArray

        common[0]

    lines
    |> Array.chunkBySize 3
    |> Array.map common
    |> Array.map prio
    |> Array.sum
