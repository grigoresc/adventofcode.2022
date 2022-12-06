module aoc.day06.solutions

open aoc.common

let ok (line: char []) =
    let s = Set(line)

    if s.Count = line.Length then
        true
    else
        false

let solve1 (lines: string []) =
    let line = lines[0]

    let sln1 =
        seq {
            for i in [ 0 .. line.Length - 3 ] do
                if ok (line[ i + 0 .. i + 3 ].ToCharArray()) then
                    yield (i + 3 + 1)
        }

    let x = sln1 |> Seq.item (0)
    print x
    x

let solve2 (lines: string []) =
    let line = lines[0]


    let sln1 =
        seq {
            for i in [ 0 .. line.Length - 13 ] do
                if ok (line[ i + 0 .. i + 13 ].ToCharArray()) then
                    yield (i + 13 + 1)
        }

    let x = sln1 |> Seq.item (0)
    print x
    x
