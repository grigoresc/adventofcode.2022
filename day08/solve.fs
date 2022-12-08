module aoc.day08.solutions

open aoc.common

let parse (lines: string []) =
    let a =
        lines
        |> Array.map (fun e ->
            Array.ofSeq (Seq.toList (e))
            |> Array.map (fun e -> int (e.ToString())))

    a


let findMax (a: int array array) (i1, iinc, i2) (j1, jinc, j2) f =
    seq {
        for i in [ i1..iinc..i2 ] do
            let mutable max = -1

            for j in [ j1..jinc..j2 ] do
                if a[i][j] > max then
                    max <- a[i][j]
                    yield f (i, j)
    }

let solve1 lines =

    let a = parse lines
    let at = Array.transpose a
    let len = a.Length

    let look =
        seq {

            yield! findMax a (0, 1, len - 1) (0, 1, len - 1) (fun (i, j) -> (i, j))
            yield! findMax a (0, 1, len - 1) (len - 1, -1, 0) (fun (i, j) -> (i, j))
            yield! findMax at (len - 1, -1, 0) (0, 1, len - 1) (fun (i, j) -> (j, i))
            yield! findMax at (len - 1, -1, 0) (len - 1, -1, 0) (fun (i, j) -> (j, i))
        }

    let la = Array.ofSeq look
    let total = Seq.distinct look |> Seq.length
    total

let calcScore (a: int array array) v i j1 jinc j2 =
    let mutable score = 0
    let mutable c = true

    for j in j2 .. (-jinc) .. j1 do
        if v > a[i][j] && c = true then
            score <- score + 1
        else
            if c then score <- score + 1
            c <- false

    score

let calcScenicView (a: int array array) (i1, iinc, i2) (j1, jinc, j2) f =
    let len = a.Length
    let score = Array2D.zeroCreate len len

    for i in [ i1..iinc..i2 ] do
        let mutable max = -1

        for j in [ j1..jinc..j2 ] do
            let v = a[i][j]

            let sco =
                match i, j with
                | (i, j) when i = 0 || j = 0 || i = len - 1 || j = len - 1 -> 0
                | _ -> calcScore a v i j1 jinc (j - jinc)

            score[fst (f (i, j)), snd (f (i, j))] <- sco

    score

let solve2 lines =

    let a = parse lines
    let at = Array.transpose a
    let len = a.Length


    let c3 = calcScenicView at (len - 1, -1, 0) (0, 1, len - 1) (fun (i, j) -> (j, i)) //top
    let c1 = calcScenicView a (0, 1, len - 1) (0, 1, len - 1) (fun (i, j) -> (i, j)) //left
    let c4 = calcScenicView at (len - 1, -1, 0) (len - 1, -1, 0) (fun (i, j) -> (j, i)) //down
    let c2 = calcScenicView a (0, 1, len - 1) (len - 1, -1, 0) (fun (i, j) -> (i, j)) //right

    let res = Array2D.zeroCreate len len

    let mutable max = 0

    for i in 0 .. len - 1 do
        for j in 0 .. len - 2 do
            let p = c1[i, j] * c2[i, j] * c3[i, j] * c4[i, j]
            if p > max then max <- p
            res[i, j] <- p

    max
