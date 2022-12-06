module aoc.day06.solutions

let ok (s: string) = Set(s).Count = s.Length

let solve size (lines: string []) =
    let line = lines[0]

    let markers =
        seq {
            for i in [ 0 .. line.Length - size + 1 ] do
                if ok (line[i + 0 .. i + size - 1]) then
                    yield (i + size)
        }

    Seq.head markers

let solve1 = solve 4
let solve2 = solve 14
