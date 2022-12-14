module aoc.day14.solutions

open aoc.common

let dir (x1, x2) = if x1 > x2 then -1 else 1

let solve1 (lines: string []) =
    let start = (500, 0)

    let rockPaths =
        lines
        |> Array.map (fun x ->
            x.Split " -> "
            |> Array.map readDigits
            |> Array.map (fun x -> x[0], x[1]))

    let maxX =
        rockPaths
        |> Array.map (fun x -> Array.map fst x)
        |> ArrayCollect
        |> Array.max

    let maxY =
        rockPaths
        |> Array.map (fun x -> Array.map snd x)
        |> ArrayCollect
        |> Array.max

    let minX =
        rockPaths
        |> Array.map (fun x -> Array.map fst x)
        |> ArrayCollect
        |> Array.min

    let minY =
        rockPaths
        |> Array.map (fun x -> Array.map snd x)
        |> ArrayCollect
        |> Array.min

    let map = Array2D.create (maxY + 1 - minY) (maxX + 1 - minX) "."

    print (minX, minY)
    print (maxX, maxY)
    print rockPaths

    rockPaths
    |> Array.iter (fun paths ->
        let mutable current = paths[0]

        for p in paths[1..] do
            print (current, p)

            for x in fst (current) .. dir (fst (current), fst (p)) .. fst (p) do
                for y in snd (current) .. dir (snd (current), snd (p)) .. snd (p) do
                    printm "set" (x, y)
                    map[y - minY, x - minX] <- "#"

            current <- p)


    printScreen map
    1

let solve2 = solve1
