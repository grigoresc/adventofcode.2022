module aoc.day14.solutions

open aoc.common

let dir (x1, x2) = if x1 > x2 then -1 else 1

type Boundary =
    { minX: int
      minY: int
      maxX: int
      maxY: int }

let boundaries (map: Set<int * int>) =

    let maxX = map |> Set.map (fun x -> fst x) |> Set.maxElement
    let maxY = map |> Set.map (fun x -> snd x) |> Set.maxElement
    let minX = map |> Set.map (fun x -> fst x) |> Set.minElement
    let minY = map |> Set.map (fun x -> snd x) |> Set.minElement

    { minX = minX
      minY = minY
      maxX = maxX
      maxY = maxY }


let printScreenM (map: Set<int * int>) =
    printm "map" map.Count

    let bnd = boundaries map
    let a = Array2D.create (bnd.maxY - bnd.minY + 1) (bnd.maxX - bnd.minX + 1) "."

    for p in map do

        a[snd (p) - bnd.minY, fst (p) - bnd.minX] <- "#"

    printScreen a

let iswall1 (b: Boundary) (x, y) = false

let iswall2 (b: Boundary) (x, y) = y = b.maxY + 1

let abyss1 (b: Boundary) (x, y) =
    not (
        b.minX <= x
        && x <= b.maxX
        && b.minY <= y
        && y <= b.maxY
    )

let abyss2 (b: Boundary) (x, y) = false

let rec dropFrom (x, y) (map: Set<int * int>) (iswall: int * int -> bool) (abyss: int * int -> bool) =
    match x, y with
    | x, y when abyss (x, y) -> None
    | x, y when iswall (x, y) -> Some(map.Add((x, y)))
    | x, y when not (map.Contains((x, y + 1))) -> dropFrom (x, y + 1) map iswall abyss
    | x, y when not (map.Contains((x - 1, y + 1))) -> dropFrom (x - 1, y + 1) map iswall abyss
    | x, y when not (map.Contains((x + 1, y + 1))) -> dropFrom (x + 1, y + 1) map iswall abyss
    | _ -> Some(map.Add((x, y)))

let read (lines: string []) =

    let rockPaths =
        lines
        |> Array.map (fun x ->
            x.Split " -> "
            |> Array.map readDigits
            |> Array.map (fun x -> x[0], x[1]))


    let mutable map = Set.empty

    rockPaths
    |> Array.iter (fun paths ->
        let mutable current = paths[0]

        for p in paths[1..] do

            for x in fst (current) .. dir (fst (current), fst (p)) .. fst (p) do
                for y in snd (current) .. dir (snd (current), snd (p)) .. snd (p) do
                    map <- map.Add((x, y))

            current <- p)

    let b = { (boundaries map) with minY = 0 } //todo can I get rid of this?

    (map, b)

let solve1 (lines: string []) =
    let (map, b) = read lines

    let mutable c = Some(map)
    let mutable lastok = map
    let mutable cnt = -1

    while not (c = None) do //todo how can I write the 'while with None check' in functional style?
        lastok <- c.Value
        cnt <- cnt + 1
        c <- dropFrom (500, 0) lastok (iswall1 b) (abyss1 b)

    printScreenM map
    printScreenM lastok
    cnt

let solve2 (lines: string []) =
    let (map, b) = read lines

    let mutable c = Some(map)
    let mutable lastok = map
    let mutable cnt = -1
    let mutable same = false

    while not same do
        lastok <- c.Value
        cnt <- cnt + 1
        c <- dropFrom (500, 0) lastok (iswall2 b) (abyss2 b)

        same <- (c.Value.Count = lastok.Count)

    printScreenM map
    printScreenM lastok
    cnt
