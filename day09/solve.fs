module aoc.day09.solutions

let directions =
    Map [ "R", (1, 0)
          "U", (0, 1)
          "L", (-1, 0)
          "D", (0, -1) ]

let moveTowards head prevTail =
    if head > prevTail then
        prevTail + 1
    else
        prevTail - 1

let nextTail tail head =

    let diff = (abs (fst (tail) - fst (head)), abs (snd (tail) - snd (head)))

    let newTail =
        match diff with
        | (0, 0) -> tail
        | (0, 1) -> tail

        | (1, 0) -> tail
        | (1, 1) -> tail

        | (0, 2) -> (fst (head), moveTowards (snd (head)) (snd (tail)))
        | (2, 0) -> (moveTowards (fst (head)) (fst (tail)), snd (head))

        | (1, 2) -> (fst (head), moveTowards (snd (head)) (snd (tail)))
        | (2, 1) -> (moveTowards (fst (head)) (fst (tail)), snd (head))

        | (2, 2) -> (moveTowards (fst (head)) (fst (tail)), moveTowards (snd (head)) (snd (tail))) //that's the extra case for 2nd part..

        | _ -> failwith $"shouldn't reach this case... diff={diff}"

    newTail

let parse (line: string) =
    let a = line.Split ' '
    (a[0], int (a[1]))

let solve (knots: (int * int) []) motions =
    let lastTailIdx = knots.Length - 1
    let mutable visited = set ([ knots[lastTailIdx] ])

    for (d, sz) in motions do
        for i in 0 .. sz - 1 do
            knots[0] <- (fst (knots[0]) + fst (directions[d]), snd (knots[0]) + snd (directions[d]))

            for tailIndex in 1..lastTailIdx do
                knots[tailIndex] <- nextTail knots[tailIndex] knots[tailIndex - 1]

            visited <- visited.Add knots[lastTailIdx]

    visited.Count

let solve1 (lines: string []) =
    let knots = Array.create 2 (0, 0)
    lines |> Array.map parse |> solve knots


let solve2 (lines: string []) =
    let knots = Array.create 10 (0, 0)
    lines |> Array.map parse |> solve knots
