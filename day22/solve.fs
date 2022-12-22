module aoc.day22.solutions

open aoc.common

type Rotate =
    | R
    | L
    | Nothing

type QuizType =
    | Input
    | Sample

let read (lines: string []) =
    let mapPart, cmdPart =
        lines
        |> splitByCond (fun line -> line = "")
        |> asTupleOf2

    let steps = cmdPart[0] |> readNumbers

    let rotates =
        cmdPart[0]
        |> readNonNumbers
        |> Array.map (fun x -> if x = "L" then Rotate.L else Rotate.R)

    let H = mapPart.Length

    let W =
        mapPart
        |> List.map (fun x -> x.Length)
        |> List.max

    let map = Array2D.create H W ' '

    for i in 0 .. H - 1 do
        for j in 0 .. W - 1 do
            if mapPart[i].Length > j then
                map[i, j] <- mapPart[i][j]

    steps, rotates, map

type Dir =
    | R
    | D
    | L
    | U

let DirChar =
    dict [ (Dir.R, '>')
           (Dir.L, '<')
           (Dir.D, 'v')
           (Dir.U, '^') ]

let solve (map: char array2d, steps: int array, rotations: Rotate array) (wrap: int * int * Dir -> int * int * Dir) =

    let (H, W) = (Array2D.length1 map, Array2D.length2 map)
    let (_, startY) = array2d_findIndex map (fun e -> e = '.' || e = '#')

    let dir = R
    let spos = (0, startY[0])
    printm "initial pos" spos
    let mapToPrint = Array2D.copy map
    let bounded (x, y) = 0 <= x && x < H && 0 <= y && y < W

    let moveFrom (x: int, y: int) (dir: Dir) (steps: int) =

        mapToPrint[x, y] <- DirChar.Item dir

        let (rx, ry, rdir) =
            ((x, y, dir), { 0 .. steps - 1 })
            ||> Seq.fold (fun (cx, cy, cdir) step ->
                let nextX, nextY =
                    match cdir with
                    | Dir.R -> cx, cy + 1
                    | Dir.L -> cx, cy - 1
                    | Dir.U -> cx - 1, cy
                    | Dir.D -> cx + 1, cy

                let (ncx, ncy, ncdir) =
                    match nextX, nextY with
                    | nextX, nextY when bounded (nextX, nextY) && map[nextX, nextY] = '.' -> nextX, nextY, cdir
                    | nextX, nextY when bounded (nextX, nextY) && map[nextX, nextY] = '#' -> cx, cy, cdir
                    | nextX, nextY when
                        bounded (nextX, nextY) && map[nextX, nextY] = ' '
                        || not (bounded (nextX, nextY))
                        ->
                        let nextWrapX, nextWrapY, nextDir = wrap (cx, cy, cdir)

                        if map[nextWrapX, nextWrapY] = '#' then
                            cx, cy, cdir
                        else
                            nextWrapX, nextWrapY, nextDir
                    | _ -> failwith "unexpected case"

                mapToPrint[ncx, ncy] <- DirChar.Item ncdir
                (ncx, ncy, ncdir))

        //printScreen false mapToPrint
        (rx, ry), rdir

    let move (pos: int * int, dir: Dir) (steps: int) (rotate: Rotate) =
        let nextPos, cdir = moveFrom pos dir steps

        let nextDir =
            match cdir, rotate with
            | _, Rotate.Nothing -> dir
            | Dir.R, Rotate.R -> Dir.D
            | Dir.R, Rotate.L -> Dir.U
            | Dir.L, Rotate.R -> Dir.U
            | Dir.L, Rotate.L -> Dir.D
            | Dir.D, Rotate.R -> Dir.L
            | Dir.D, Rotate.L -> Dir.R
            | Dir.U, Rotate.R -> Dir.R
            | Dir.U, Rotate.L -> Dir.L

        nextPos, nextDir

    let (ifinalPos, ifinalDir) =
        ((spos, Dir.R), steps, rotations)
        |||> Seq.fold2 move

    let (finalPos, finalDir) =
        move (ifinalPos, ifinalDir) (steps[steps.Length - 1]) Rotate.Nothing

    printm "final pos" finalPos

    let dvalue =
        match finalDir with
        | Dir.R -> 0
        | Dir.L -> 2
        | Dir.D -> 1
        | Dir.U -> 3

    let sln =
        1000 * (fst (finalPos) + 1)
        + 4 * (snd (finalPos) + 1)
        + dvalue

    printMatrix false mapToPrint
    print sln
    sln


let solve1 (lines: string []) =
    let steps, rotations, map = read lines

    let startX, startY = array2d_findIndex map (fun e -> e = '.' || e = '#')
    let endX, endY = array2d_findIndexBack map (fun e -> e = '.' || e = '#')

    let wrap1 (cx, cy, dir) =
        match dir with
        | Dir.R -> cx, startY[cx], dir
        | Dir.L -> cx, endY[cx], dir
        | Dir.U -> endX[cy], cy, dir
        | Dir.D -> startX[cy], cy, dir

    let sln = solve (map, steps, rotations) wrap1
    sln

type next =
    | Zero
    | Same
    | Sameinverted
    | Oposite
    | OpositeInverted
    | Last

type RealCoord = { X: int; Y: int }

let solve2ByRules facesGrid rules (lines: string []) =
    let steps, rotations, map = read lines

    let H, W = Array2D.length1 map, Array2D.length2 map
    let gridH, gridW = Array2D.length1 facesGrid, Array2D.length2 facesGrid

    let SZ = H / gridH
    assert (SZ = W / gridW)

    let compNext (nextX: next) (nextY: next) (c: RealCoord) =
        let cnextX =
            match nextX with
            | Zero -> 0
            | Same -> c.X % SZ
            | Sameinverted -> SZ - 1 - c.X % SZ
            | Oposite -> c.Y % SZ
            | OpositeInverted -> SZ - 1 - c.Y % SZ
            | Last -> SZ - 1

        let cnextY =
            match nextY with
            | Zero -> 0
            | Same -> c.Y % SZ
            | Sameinverted -> SZ - 1 - c.Y % SZ
            | Oposite -> c.X % SZ
            | OpositeInverted -> SZ - 1 - c.X % SZ
            | Last -> SZ - 1

        { X = cnextX; Y = cnextY }

    let faceByPos x y = facesGrid[int (x / SZ), int (y / SZ)]

    let posByFace face = //todo - any other solution?
        let (x, y, v) =
            facesGrid
            |> Array2D.mapi (fun i j v -> (i, j, v))
            |> toArray
            |> Array.find (fun (x, y, v) -> v = face)

        x, y

    let getNext (face: int) (dir: Dir) (c: RealCoord) =
        let (nextFace, nextDir, (xMove, yMove)) = rules face dir
        nextFace, nextDir, compNext xMove yMove c

    let faces = Array2D.init H W faceByPos
    printMatrix false faces

    let nextFace (cx, cy, dir) =
        let face = faceByPos cx cy

        let nextopBasedOn_Y_X_System (face, dir, (x, y)) = //!!!! I need to invert X with Y here...
            let c = { X = y; Y = x } //!!!!
            let nextface, nextDir, nextCoord = getNext face dir c
            (nextface, nextDir, nextCoord.Y, nextCoord.X) //!!!!

        let nface, ndir, x, y = nextopBasedOn_Y_X_System (face, dir, (cx, cy))

        let origpos = posByFace nface
        let nx, ny = x % SZ + fst (origpos) * SZ, y % SZ + snd (origpos) * SZ
        //printm $"recalc for pos {cx},{cy}: nface={nface} origpos={origpos} x={x} y={y}; also newdir={ndir}" (nx, ny)
        (nx, ny, ndir)

    let wrap2 (cx, cy, dir) = nextFace (cx, cy, dir)

    let sln = solve (map, steps, rotations) wrap2
    sln

let solve2 (quiz: QuizType) =

    match quiz with
    | Sample ->
        let facesGrid =
            array2D [ [ 0; 0; 1; 0 ]
                      [ 2; 3; 4; 0 ]
                      [ 0; 0; 5; 6 ] ]

        let rules (face: int) (dir: Dir) =
            match face, dir with
            | 1, Dir.U -> 2, Dir.D, (Sameinverted, Zero)
            | 1, Dir.R -> 6, Dir.L, (Last, Sameinverted)
            | 1, Dir.D -> 4, Dir.D, (Same, Zero)
            | 1, Dir.L -> 3, Dir.D, (Oposite, Zero)

            | 2, Dir.U -> 1, Dir.D, (Sameinverted, Zero)
            | 2, Dir.R -> 3, Dir.R, (Zero, Same)
            | 2, Dir.D -> 5, Dir.U, (Sameinverted, Last)
            | 2, Dir.L -> 6, Dir.U, (OpositeInverted, Last)

            | 3, Dir.U -> 1, Dir.R, (Zero, Oposite)
            | 3, Dir.R -> 4, Dir.R, (Zero, Same)
            | 3, Dir.D -> 5, Dir.R, (Zero, OpositeInverted)
            | 3, Dir.L -> 2, Dir.L, (Last, Same)

            | 4, Dir.U -> 1, Dir.U, (Same, Last)
            | 4, Dir.R -> 6, Dir.D, (OpositeInverted, Zero)
            | 4, Dir.D -> 5, Dir.D, (Same, Zero)
            | 4, Dir.L -> 3, Dir.L, (Last, Same)

            | 5, Dir.U -> 4, Dir.U, (Same, Last)
            | 5, Dir.R -> 6, Dir.R, (Zero, Same)
            | 5, Dir.D -> 2, Dir.U, (Sameinverted, Last)
            | 5, Dir.L -> 3, Dir.U, (OpositeInverted, Last)

            | 6, Dir.U -> 4, Dir.L, (Last, OpositeInverted)
            | 6, Dir.R -> 1, Dir.L, (Last, Sameinverted)
            | 6, Dir.D -> 2, Dir.R, (Zero, OpositeInverted)
            | 6, Dir.L -> 5, Dir.L, (Last, Same)

            | _ -> failwith $"missed a case here...! dir={dir},face={face}"

        solve2ByRules facesGrid rules
    | Input ->
        let facesGrid =
            array2D [ [ 0; 1; 2 ]
                      [ 0; 3; 0 ]
                      [ 4; 5; 0 ]
                      [ 6; 0; 0 ] ]

        let rules (face: int) (dir: Dir) =
            match face, dir with
            | 1, Dir.U -> 6, Dir.R, (Zero, Oposite)
            | 1, Dir.R -> 2, Dir.R, (Zero, Same)
            | 1, Dir.D -> 3, Dir.D, (Same, Zero)
            | 1, Dir.L -> 4, Dir.R, (Zero, Sameinverted)

            | 2, Dir.U -> 6, Dir.U, (Same, Last)
            | 2, Dir.R -> 5, Dir.L, (Last, Sameinverted)
            | 2, Dir.D -> 3, Dir.L, (Last, Oposite)
            | 2, Dir.L -> 1, Dir.L, (Last, Same)

            | 3, Dir.U -> 1, Dir.U, (Same, Last)
            | 3, Dir.R -> 2, Dir.U, (Oposite, Last)
            | 3, Dir.D -> 5, Dir.D, (Same, Zero)
            | 3, Dir.L -> 4, Dir.D, (Oposite, Zero)

            | 4, Dir.U -> 3, Dir.R, (Zero, Oposite)
            | 4, Dir.R -> 5, Dir.R, (Zero, Same)
            | 4, Dir.D -> 6, Dir.D, (Same, Zero)
            | 4, Dir.L -> 1, Dir.R, (Zero, Sameinverted)

            | 5, Dir.U -> 3, Dir.U, (Same, Last)
            | 5, Dir.R -> 2, Dir.L, (Last, Sameinverted)
            | 5, Dir.D -> 6, Dir.L, (Last, Oposite)
            | 5, Dir.L -> 4, Dir.L, (Last, Same)

            | 6, Dir.U -> 4, Dir.U, (Same, Last)
            | 6, Dir.R -> 5, Dir.U, (Oposite, Last)
            | 6, Dir.D -> 2, Dir.D, (Same, Zero)
            | 6, Dir.L -> 1, Dir.D, (Oposite, Zero)

            | _ -> failwith $"missed a case here...! dir={dir},face={face}"

        solve2ByRules facesGrid rules
