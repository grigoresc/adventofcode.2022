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

let solve1 (lines: string []) =

    let H = lines.Length
    let W = lines[0].Length
    //print H
    //print W

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

    //printm "edges" edges
    //printm "sz" edges.Count
    //print edges[sc]
    //print edges[ec]


    let indexof coord (lst: Coord list) = List.findIndex ((=) coord) lst

    let nodes =
        edges
        |> Map.keys
        //|> Seq.map (fun x -> (x, H * W * 100))
        //|> Map.ofSeq
        |> List.ofSeq

    let distances = Array.create nodes.Length (W * H * 1000) //is that ok for part2 ?
    printm "sc" (sc, indexof sc nodes)
    distances[indexof sc nodes] <- 0
    //printm "dist ini" distances
    let mutable stack = [ (0, sc) ]

    let mutable inc = 0

    let mutable visited = Set.empty

    while (not stack.IsEmpty) && inc < 10000 do

        stack <- List.sortBy (fun o -> fst (o)) stack
        inc <- inc + 1
        //print stack
        let (dist, toprocess) = stack.Head
        let idxToProcess = indexof toprocess nodes
        stack <- stack.Tail
        //print stack

        //printm "ec" distances[indexof ec nodes]

        visited <- visited.Add idxToProcess

        //if (Seq.ofList processed)
        //   |> Seq.contains idxToProcess then
        //    printm "contain" idxToProcess

        //processed <- idxToProcess :: processed
        //print (visited |> Set.toArray)

        for nei in edges[toprocess] do
            let idxNei = indexof nei.coord nodes

            if not (visited.Contains idxNei) || false then
                //printm "cmp" (idxToProcess, idxNei)
                //let newdistance = distances[idxToProcess] + nei.cost
                let newdistance = dist + nei.cost

                if newdistance < distances[idxNei] then
                    distances[idxNei] <- newdistance
                    stack <- (newdistance, nei.coord) :: stack
    //else
    //print $"already visited {idxNei}"

    printm "inc" inc
    //printm "dist fin" distances

    printm "ec" (ec, indexof ec nodes)
    let sln = distances[indexof ec nodes]
    print sln
    sln

let solve2 (lines: string []) = 2
