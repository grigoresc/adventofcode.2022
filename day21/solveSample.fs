module aoc.day21.solutions.sample

open day21.Sample

let solveSample =

    let sln1 = root (fun a b -> a + b) 5


    let sln2 =
        [ 0..500 ]
        |> List.skipWhile (fun x ->
            let r = root (fun a b -> a = b) x

            not r)
        |> List.head

    sln1, sln2
