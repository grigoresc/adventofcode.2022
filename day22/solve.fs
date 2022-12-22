﻿module aoc.day22.solutions

open aoc.common

type Rotate =
    | R
    | L
    | Nothing

let read (lines: string []) =

    let s = lines |> splitByCond (fun line -> line = "")
    //print s
    let inst = s[1]
    let n = inst[0] |> readDigits

    let l =
        inst[0]
        |> readNonDigits
        |> Array.map (fun x -> if x = "L" then Rotate.L else Rotate.R)

    //printm "inst" (n, l)

    let m = s[0]
    let H = m.Length
    let W = m |> List.map (fun x -> x.Length) |> List.max
    //print (H, W)
    let map = Array2D.create H W ' '

    for i in 0 .. H - 1 do
        for j in 0 .. W - 1 do
            if m[i].Length > j then
                map[i, j] <- m[i][j]

    //print map
    (H, W), (n, l), map

type Dir =
    | R
    | D
    | L
    | U


let solve (map: char array2d, H, W, instN: int array, instL: Rotate array) (wrap: int * int -> Dir -> int * int) =
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

        let dirch =
            match dir with
            | Dir.R -> '>'
            | Dir.L -> '<'
            | Dir.D -> 'v'
            | Dir.U -> '^'

        mapdraw[cx, cy] <- dirch

        for i in [ 0 .. step - 1 ] do

            let nextX, nextY =
                match dir with
                | Dir.R -> cx, cy + 1
                | Dir.L -> cx, cy - 1
                | Dir.U -> cx - 1, cy
                | Dir.D -> cx + 1, cy

            //todo how to assign mutable tuple? ("Undefined value 'copyOfStruct'" error)
            let (ncx, ncy) =
                match nextX, nextY with
                | nextX, nextY when bounded (nextX, nextY) && map[nextX, nextY] = '.' -> nextX, nextY
                | nextX, nextY when bounded (nextX, nextY) && map[nextX, nextY] = '#' -> cx, cy
                | nextX, nextY when
                    bounded (nextX, nextY) && map[nextX, nextY] = ' '
                    || not (bounded (nextX, nextY))
                    ->
                    let nextWrapX, nextWrapY = wrap (cx, cy) dir

                    if map[nextWrapX, nextWrapY] = '#' then
                        cx, cy
                    else
                        nextWrapX, nextWrapY
                | _ -> failwith "unexpected case"

            cx <- ncx
            cy <- ncy

            let dirch =
                match dir with
                | Dir.R -> '>'
                | Dir.L -> '<'
                | Dir.D -> 'v'
                | Dir.U -> '^'

            mapdraw[cx, cy] <- dirch

        printScreen2 false mapdraw
        cx, cy

    let move (pos: int * int, dir: Dir) (n: int) (rotate: Rotate) =
        printm $"move from pos {pos} with dir {dir} with next steps" (n, rotate)
        let next = movefrom pos n dir
        print $"should move to {next}"


        let nextDir =
            match dir, rotate with
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

    //print startY
    //print endY
    //print startX
    //print endX


    let wrap1 (cx, cy) dir =
        match dir with
        | Dir.R -> cx, startY[cx]
        | Dir.L -> cx, endY[cx]
        | Dir.U -> endX[cy], cy
        | Dir.D -> startX[cy], cy

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

//type CubeFace = {Number:int}
type RealCoord = { X: int; Y: int }

let solve2 (lines: string []) =
    let (H, W), (instN, instL), map = read lines

    let sampleGrid =
        array2D [ [ 0; 0; 1; 0 ]
                  [ 2; 3; 4; 0 ]
                  [ 0; 0; 5; 6 ] ]

    let gridW = Array2D.length2 sampleGrid
    let gridH = Array2D.length1 sampleGrid

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
        let f = sampleGrid[int (x / SZ), int (y / SZ)]
        printm $"face for {x},{y} is" f
        f

    let posByFace face =
        let (x, y, v) =
            sampleGrid
            |> Array2D.mapi (fun i j v -> (i, j, v))
            |> toArray
            |> Array.find (fun (x, y, v) -> v = face)

        x, y

    let faces = Array2D.init H W faceByPos
    //print faces

    let nextface (cx, cy) dir =
        let face = faceByPos cx cy

        let nextopBasedOn_Y_X_System (dir, face, (x, y)) = //!!!!
            let c = { X = y; Y = x } //!!!!

            let comp =
                match dir, face with
                | Dir.U, 1 -> 2, compNext Sameinverted Zero c
                | Dir.R, 1 -> 6, compNext Last Sameinverted c
                | Dir.D, 1 -> 4, compNext Same Zero c
                | Dir.L, 1 -> 3, compNext Oposite Zero c

                | Dir.U, 2 -> 1, compNext Sameinverted Zero c
                | Dir.R, 2 -> 3, compNext Zero Same c
                | Dir.D, 2 -> 5, compNext Sameinverted Last c
                | Dir.L, 2 -> 6, compNext OpositeInverted Last c

                | Dir.U, 3 -> 1, compNext Zero Oposite c
                | Dir.R, 3 -> 4, compNext Zero Same c
                | Dir.D, 3 -> 5, compNext Zero OpositeInverted c
                | Dir.L, 3 -> 2, compNext Last Same c

                | Dir.U, 4 -> 1, compNext Same Last c
                | Dir.R, 4 -> 6, compNext OpositeInverted Zero c
                | Dir.D, 4 -> 5, compNext Same Zero c
                | Dir.L, 4 -> 3, compNext Last Same c

                | Dir.U, 5 -> 4, compNext Same Last c
                | Dir.R, 5 -> 6, compNext Zero Same c
                | Dir.D, 5 -> 2, compNext Sameinverted Last c
                | Dir.L, 5 -> 3, compNext OpositeInverted Last c

                | Dir.U, 6 -> 4, compNext Last OpositeInverted c
                | Dir.R, 6 -> 1, compNext Last Sameinverted c
                | Dir.D, 6 -> 2, compNext Zero OpositeInverted c
                | Dir.L, 6 -> 5, compNext Last Same c

                | _ -> failwith $"missed a case here...! dir={dir},face={face}"

            (fst (comp), (snd (comp)).Y, (snd (comp)).X) //!!!!

        let nface, x, y = nextopBasedOn_Y_X_System (dir, face, (cx, cy))

        let origpos = posByFace nface
        let nx, ny = x % SZ + fst (origpos) * SZ, y % SZ + snd (origpos) * SZ
        printm $"recalc for pos {cx},{cy}: nface={nface} origpos={origpos} x={x} y={y}" (nx, ny)
        (nx, ny)

    let wrap (cx, cy) dir = nextface (cx, cy) dir

    //print wrap
    let sln = solve (map, H, W, instN, instL) wrap
    sln
//print nextface
//1