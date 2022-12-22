[<AutoOpen>]
module aoc.common.draw

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

let printScreenM14 (map: Set<int * int>) =
    printm "map" map.Count

    let bnd = boundaries map
    let a = Array2D.create (bnd.maxY - bnd.minY + 1) (bnd.maxX - bnd.minX + 1) "."

    for p in map do

        a[snd (p) - bnd.minY, fst (p) - bnd.minX] <- "#"

    printMatrix false a

let printScreenM17 (boundary: Boundary) (upsidedown: bool) (map: Set<int * int>) =
    printm "map" map.Count

    let bnd =
        boundaries (
            map
        //+ Set(
        //    [ (boundary.minX, boundary.minY)
        //      (boundary.maxX, boundary.maxY) ]
        //)
        )
    //print bnd
    let a = Array2D.create (bnd.maxY - bnd.minY + 1) (bnd.maxX - bnd.minX + 1) "."

    for p in map do
        a[snd (p) - bnd.minY, fst (p) - bnd.minX] <- "#"

    //print a
    printMatrix upsidedown a
