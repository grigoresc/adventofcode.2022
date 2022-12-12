module aoc.day12.solutions

open aoc.common

let Dirs = [ (0, 1); (1, 0); (0, -1); (-1, 0) ]

let Pos i j H W =
    [ for dir in Dirs do
          let ni = i + fst (dir)
          let nj = j + snd (dir)

          if 0 <= ni && ni < H && 0 <= nj && nj < W then
              yield (ni, nj) ]

type Coord = { X: int; Y: int }

type CostToCoord = { coord: Coord; cost: int }

let OnlyAllowed (lines: string []) (coord1: Coord) (coord2: CostToCoord) =
    let mutable c1 = lines[coord1.X][coord1.Y]
    let mutable c2 = lines[coord2.coord.X][coord2.coord.Y]

    if c1 = 'S' then c1 <- 'a'
    if c1 = 'E' then c1 <- 'z'
    if c2 = 'S' then c2 <- 'a'
    if c2 = 'E' then c2 <- 'z'

    if int (c1) + 1 = int (c2) || int (c1) >= int (c2) then
        true
    else
        false

let OnlyAllowed2 (lines: string []) (coord1: Coord) (coord2: CostToCoord) =
    let mutable c1 = lines[coord1.X][coord1.Y]
    let mutable c2 = lines[coord2.coord.X][coord2.coord.Y]

    if c1 = 'S' then c1 <- 'a'
    if c1 = 'E' then c1 <- 'z'
    if c2 = 'S' then c2 <- 'a'
    if c2 = 'E' then c2 <- 'z'

    if int (c2) + 1 = int (c1) || int (c2) >= int (c1) then
        true
    else
        false

let indexof coord (lst: Coord list) = List.findIndex ((=) coord) lst

let minCost (edges: Map<Coord, CostToCoord list>) (nodes: Coord list) sc =
    let distances = Array.create nodes.Length (System.Int32.MaxValue)
    distances[indexof sc nodes] <- 0
    let mutable stack = [ (0, sc) ]

    while (not stack.IsEmpty) do
        stack <- List.sortBy (fun o -> fst (o)) stack
        let (dist, toprocess) = stack.Head
        stack <- stack.Tail

        for nei in edges[toprocess] do
            let idxNei = indexof nei.coord nodes
            let newdistance = dist + nei.cost

            if newdistance < distances[idxNei] then
                distances[idxNei] <- newdistance
                stack <- (newdistance, nei.coord) :: stack

    distances

let solve1 (lines: string []) =
    let H = lines.Length
    let W = lines[0].Length

    let mutable sc = { X = -1; Y = -1 }
    let mutable ec = { X = -1; Y = -1 }

    let edges =
        seq {
            for i in 0 .. H - 1 do
                for j in 0 .. W - 1 do

                    let nei =
                        [ for pos in Pos i j H W do
                              //print (i, j, fst (pos), snd (pos))
                              if lines[i][j] = 'S' then
                                  sc <- { X = i; Y = j }

                              if lines[i][j] = 'E' then
                                  ec <- { X = i; Y = j }

                              yield
                                  { cost = 1
                                    coord = { X = fst (pos); Y = snd (pos) } } ]
                        |> List.filter (OnlyAllowed lines { X = i; Y = j })

                    yield { X = i; Y = j }, nei
        }
        |> Map.ofSeq

    let nodes = edges |> Map.keys |> List.ofSeq

    let distances = minCost edges nodes sc

    let sln = distances[indexof ec nodes]
    print sln
    sln

let solve2 (lines: string []) =
    let H = lines.Length
    let W = lines[0].Length

    let mutable sc = { X = -1; Y = -1 }
    let mutable ec = { X = -1; Y = -1 }

    let incomingEdges =
        seq {
            for i in 0 .. H - 1 do
                for j in 0 .. W - 1 do

                    let nei =
                        [ for pos in Pos i j H W do
                              if lines[i][j] = 'S' then
                                  sc <- { X = i; Y = j }

                              if lines[i][j] = 'E' then
                                  ec <- { X = i; Y = j }

                              yield
                                  { cost = 1
                                    coord = { X = fst (pos); Y = snd (pos) } } ]
                        |> List.filter (OnlyAllowed2 lines { X = i; Y = j })

                    yield { X = i; Y = j }, nei
        }
        |> Map.ofSeq

    let nodes = incomingEdges |> Map.keys |> List.ofSeq

    let distances = minCost incomingEdges nodes ec

    let minDistance =
        nodes
        |> List.filter (fun c -> lines[c.X][c.Y] = 'a' || lines[c.X][c.Y] = 'S')
        |> List.map (fun c -> distances[indexof c nodes])
        |> List.min

    print minDistance
    minDistance
