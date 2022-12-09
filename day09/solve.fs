module aoc.day09.solutions

let directions =
    Map [ "R", (1, 0)
          "U", (0, 1)
          "L", (-1, 0)
          "D", (0, -1) ]

let moveTowards newH posT =
    if newH > posT then
        posT + 1
    else
        posT - 1

let move posH posT newH =

    let diff = (abs (fst (posT) - fst (newH)), abs (snd (posT) - snd (newH)))

    let newT =
        match diff with
        | (0, 0) -> posT
        | (0, 1) -> posT

        | (1, 0) -> posT
        | (1, 1) -> posT

        | (0, 2) -> (fst (newH), moveTowards (snd (newH)) (snd (posT)))
        | (2, 0) -> (moveTowards (fst (newH)) (fst (posT)), snd (newH))

        | (1, 2) -> (fst (posH), moveTowards (snd (newH)) (snd (posT)))
        | (2, 1) -> (moveTowards (fst (newH)) (fst (posT)), snd (posH))

        | (2, 2) -> (moveTowards (fst (newH)) (fst (posT)), moveTowards (snd (newH)) (snd (posT))) //that's the extra case for 2nd part..

        | _ -> failwith $"shouldn't reach this case... diff={diff}"

    newH, newT

let parse (line: string) =
    let a = line.Split ' '
    (a[0], int (a[1]))

let solve (knots: (int * int) []) motions =
    let tailIdx = knots.Length - 1
    let mutable visited = set ([ knots[tailIdx] ])

    for (d, sz) in motions do

        for i in 0 .. sz - 1 do

            let mutable newH =
                (fst (knots[0]) + fst (directions[d]), snd (knots[0]) + snd (directions[d]))

            for idxH in 0 .. tailIdx - 1 do
                let idxT = idxH + 1

                let (nposH, nposT) = move knots[idxH] knots[idxT] newH
                knots[idxH] <- nposH
                knots[idxT] <- nposT

                newH <- nposT

            visited <- visited.Add knots[tailIdx]


    visited.Count

let solve1 (lines: string []) =
    let knots = Array.create 2 (0, 0)
    lines |> Array.map parse |> solve knots


let solve2 (lines: string []) =
    let knots = Array.create 10 (0, 0)
    lines |> Array.map parse |> solve knots
