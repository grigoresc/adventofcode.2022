module aoc.day06.solutions

open aoc.common

let ok (line: string) = Set(line).Count = line.Length

let solve size (lines: string []) =
    let line = lines[0]

    let sln =
        seq {
            for i in [ 0 .. line.Length - size + 1 ] do
                if ok (line[i + 0 .. i + size - 1]) then
                    yield (i + size)
        }

    let x = sln |> Seq.item (0)
    x

let solve1 = solve 4
let solve2 = solve 14
