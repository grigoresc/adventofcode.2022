module aoc.day10.solutions

type Cmd =
    | Noop
    | AddX of int

let parse (line: string) =
    let a = line.Split ' '

    match a with
    | a when a[0] = "noop" -> Noop
    | a when a[0] = "addx" -> AddX(int (a[1]))
    | _ -> failwith "cannot parse"

let execute commands =
    seq {
        let mutable c = 1

        for cmd in commands do
            match cmd with
            | Noop -> yield c
            | AddX (x) ->
                yield c
                c <- c + x
                yield c
    }


let solve1 (lines: string []) =
    let registry = lines |> Seq.map parse |> execute |> Seq.toArray

    let idx = [| 20; 60; 100; 140; 180; 220 |]

    let sum =
        idx
        |> Array.map (fun cy -> cy * registry[cy - 1 - 1])
        |> Array.sum

    sum

let solve2 (lines: string []) =
    let registry = lines |> Seq.map parse |> execute |> Seq.toArray
    let screen = Array2D.create 6 40 "."

    for x in 0 .. 6 - 1 do
        for y in 0 .. 40 - 1 do
            let reg =
                if x > 0 || y > 0 then
                    registry[x * 40 + y - 1]
                else
                    1

            if reg - 1 <= y && y <= reg + 1 then
                screen[x, y] <- "#"

    screen
