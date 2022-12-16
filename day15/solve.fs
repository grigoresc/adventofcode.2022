module aoc.day15.solutions

open aoc.common

let Dirs = [ (1, 1); (1, -1); (-1, 1); (-1, -1) ]
type Coord = { X: int; Y: int }

let fromTo x1 x2 =
    if x1 = x2 then [ x1 ]
    else if x1 < x2 then [ x1..1..x2 ]
    else [ x2 .. -1 .. x1 ]

let toRow di x1 y1 yi =

    let s =
        seq {
            for d in Dirs do
                let co =
                    (fromTo y1 (y1 + di * snd (d)))
                    |> List.contains yi

                let co = true

                if co then
                    for step in 0..di do

                        let x2 = x1 + step * fst (d)
                        let y2 = y1 + (di - step) * snd (d)

                        if y2 = yi then
                            //print (x2,y2)
                            yield { X = x2; Y = y2 }
        }

    s

let diamont (c1: Coord) (c2: Coord) row =
    let d = abs (c1.X - c2.X) + abs (c1.Y - c2.Y)
    //print (c1, c2, d)
    let yi = row

    Set.ofSeq
    <| seq {
        for di in 1..d do
            yield! toRow di c1.X c1.Y yi
    }

let solve1 row (lines: string []) =

    let coords =
        lines
        |> Array.map readDigits
        |> Array.map (fun x -> { X = x[0]; Y = x[1] }, { X = x[2]; Y = x[3] })

    let allcoords =
        coords
        |> Array.map (fun x -> [ fst (x); snd (x) ])
        |> ArrayCollect
        |> Set.ofArray


    let sets =
        coords
        |> Seq.mapi (fun i c ->
            //printm $"i" i
            let s = diamont (fst (c)) (snd (c)) row
            //printm $"s" (s |> Seq.toArray)
            s)

    let all = sets |> Set.unionMany
    let alle = Set.difference all allcoords

    //print (Array.ofSeq all)
    printm "cnt" alle.Count

    let sln = alle.Count
    sln

let solve2 = solve1
