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
    let s = lines |> splitByCond (fun line -> line = "")
    let inst = s[1]
    let n = inst[0] |> readDigits

    let l =
        inst[0]
        |> readNonDigits
        |> Array.map (fun x -> if x = "L" then Rotate.L else Rotate.R)

    let m = s[0]
    let H = m.Length
    let W = m |> List.map (fun x -> x.Length) |> List.max
    let map = Array2D.create H W ' '

    for i in 0 .. H - 1 do
        for j in 0 .. W - 1 do
            if m[i].Length > j then
                map[i, j] <- m[i][j]

    (H, W), (n, l), map

type Dir =
    | R
    | D
    | L
    | U

let solve (map: char array2d, H, W, instN: int array, instL: Rotate array) (wrap: int * int * Dir -> int * int * Dir) =
    //todo - not its place here..
    let startY =
        [ 0 .. H - 1 ]
        |> List.map (fun x ->
            (map[x, *])
            |> Array.findIndex (fun e -> e = '.' || e = '#'))

    let dir = R
    let spos = (0, startY[0])
    printm "start pos" spos
    let mapdraw = Array2D.copy map
    let bounded (x, y) = 0 <= x && x < H && 0 <= y && y < W

    let movefrom (x: int, y: int) (step: int) (dir: Dir) =
        let mutable cx = x
        let mutable cy = y
        let mutable cdir = dir

        let dirch =
            match cdir with
            | Dir.R -> '>'
            | Dir.L -> '<'
            | Dir.D -> 'v'
            | Dir.U -> '^'

        mapdraw[cx, cy] <- dirch

        for i in [ 0 .. step - 1 ] do

            let nextX, nextY =
                match cdir with
                | Dir.R -> cx, cy + 1
                | Dir.L -> cx, cy - 1
                | Dir.U -> cx - 1, cy
                | Dir.D -> cx + 1, cy

            //todo how to assign mutable tuple? ("Undefined value 'copyOfStruct'" error)
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

            cx <- ncx
            cy <- ncy
            cdir <- ncdir

            let dirch =
                match cdir with
                | Dir.R -> '>'
                | Dir.L -> '<'
                | Dir.D -> 'v'
                | Dir.U -> '^'

            mapdraw[cx, cy] <- dirch

        //printScreen2 false mapdraw
        (cx, cy), cdir

    let move (pos: int * int, dir: Dir) (n: int) (rotate: Rotate) =
        printm $"move from pos {pos} with dir {dir} with next steps" (n, rotate)
        let next, cdir = movefrom pos n dir
        print $"moved to {next}"


        let nextDir =
            match cdir, rotate with
            | _, Rotate.Nothing -> dir
            | Dir.R, _ ->
                if rotate = Rotate.R then
                    Dir.D
                else
                    Dir.U
            | Dir.L, _ ->
                if rotate = Rotate.R then
                    Dir.U
                else
                    Dir.D
            | Dir.D, _ ->
                if rotate = Rotate.R then
                    Dir.L
                else
                    Dir.R
            | Dir.U, _ ->
                if rotate = Rotate.R then
                    Dir.R
                else
                    Dir.L

        //print (next, nextDir)
        next, nextDir

    let (ifinalPos, ifinalDir) = ((spos, Dir.R), instN, instL) |||> Seq.fold2 move

    let (finalPos, finalDir) =
        move (ifinalPos, ifinalDir) (instN[instN.Length - 1]) Rotate.Nothing

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

    print sln
    sln

let solve1 (lines: string []) =
    let (H, W), (instN, instL), map = read lines

    let startY =
        [ 0 .. H - 1 ]
        |> List.map (fun x ->
            (map[x, *])
            |> Array.findIndex (fun e -> e = '.' || e = '#'))

    let endY =
        [ 0 .. H - 1 ]
        |> List.map (fun x ->
            (map[x, *])
            |> Array.findIndexBack (fun e -> e = '.' || e = '#'))

    let startX =
        [ 0 .. W - 1 ]
        |> List.map (fun x ->
            (map[*, x])
            |> Array.findIndex (fun e -> e = '.' || e = '#'))

    let endX =
        [ 0 .. W - 1 ]
        |> List.map (fun x ->
            (map[*, x])
            |> Array.findIndexBack (fun e -> e = '.' || e = '#'))

    let wrap1 (cx, cy, dir) =
        match dir with
        | Dir.R -> cx, startY[cx], dir
        | Dir.L -> cx, endY[cx], dir
        | Dir.U -> endX[cy], cy, dir
        | Dir.D -> startX[cy], cy, dir

    print wrap1
    let sln = solve (map, H, W, instN, instL) wrap1
    sln

type next =
    | Zero
    | Same
    | Sameinverted
    | Oposite
    | OpositeInverted
    | Last

type RealCoord = { X: int; Y: int }

let solve2_ facesGrid rules (lines: string []) =
    let (H, W), (instN, instL), map = read lines

    let gridW = Array2D.length2 facesGrid
    let gridH = Array2D.length1 facesGrid

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

    let faceByPos x y =
        let f = facesGrid[int (x / SZ), int (y / SZ)]
        //printm $"face for {x},{y} is" f
        f

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
    //print faces

    let nextface (cx, cy, dir) =
        let face = faceByPos cx cy

        let nextopBasedOn_Y_X_System (face, dir, (x, y)) = //!!!! I need to invert X with Y here...
            let c = { X = y; Y = x } //!!!!
            let nextface, nextDir, nextCoord = getNext face dir c
            (nextface, nextDir, nextCoord.Y, nextCoord.X) //!!!!

        let nface, ndir, x, y = nextopBasedOn_Y_X_System (face, dir, (cx, cy))

        let origpos = posByFace nface
        let nx, ny = x % SZ + fst (origpos) * SZ, y % SZ + snd (origpos) * SZ
        printm $"recalc for pos {cx},{cy}: nface={nface} origpos={origpos} x={x} y={y}; also newdir={ndir}" (nx, ny)
        (nx, ny, ndir)

    let wrap (cx, cy, dir) = nextface (cx, cy, dir)

    //print wrap
    let sln = solve (map, H, W, instN, instL) wrap
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

        solve2_ facesGrid rules
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

        solve2_ facesGrid rules
