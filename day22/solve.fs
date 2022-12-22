module aoc.day22.solutions

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


let solve (lines: string []) =
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

    let spos = (0, startY[0])
    printm "spos" spos
    let dir = R

    let bounded (x, y) = 0 <= x && x < H && 0 <= y && y < W

    let moveto (x: int, y: int) (step: int) (dir: Dir) =
        let mutable cx = x
        let mutable cy = y

        for i in [ 0 .. step - 1 ] do

            let nextX, nextY, nextWrapX, nextWrapY =
                match dir with
                | Dir.R -> cx, cy + 1, cx, startY[cx]
                | Dir.L -> cx, cy - 1, cx, endY[cx]
                | Dir.U -> cx - 1, cy, endX[cy], cy
                | Dir.D -> cx + 1, cy, startX[cy], cy

            //todo how to assign mutable tuple? ("Undefined value 'copyOfStruct'" error)
            let (ncx, ncy) =
                match nextX, nextY with
                | nextX, nextY when bounded (nextX, nextY) && map[nextX, nextY] = '.' -> nextX, nextY
                | nextX, nextY when bounded (nextX, nextY) && map[nextX, nextY] = '#' -> cx, cy
                | nextX, nextY when
                    bounded (nextX, nextY) && map[nextX, nextY] = ' '
                    || not (bounded (nextX, nextY))
                    ->
                    if map[nextWrapX, nextWrapY] = '#' then
                        cx, cy
                    else
                        nextWrapX, nextWrapY
                | _ -> failwith "unexpected case"

            cx <- ncx
            cy <- ncy

        cx, cy

    let move (pos: int * int, dir: Dir) (n: int) (rotate: Rotate) =
        printm $"move from {pos} and {dir} with" (n, rotate)
        let next = moveto pos n dir

        match dir with
        | Dir.R -> print ">"
        | Dir.L -> print "<"
        | Dir.D -> print "v"
        | Dir.U -> print "^"


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

        print (next, nextDir)
        next, nextDir

    let (ifinalPos, ifinalDir) = ((spos, Dir.R), instN, instL) |||> Seq.fold2 move

    let (finalPos, finalDir) =
        move (ifinalPos, ifinalDir) (instN[instN.Length - 1]) Rotate.Nothing

    print finalPos

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

let solve1 = solve
let solve2 = solve
