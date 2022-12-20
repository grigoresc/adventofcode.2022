module aoc.day20.solutions

open aoc.common

let circularPos (sz: int) (pos: int) =
    if 0 <= pos && pos <= sz - 1 then
        pos
    else if pos < 0 then
        (sz - (abs (pos) % sz)) % sz
    else
        pos % sz

let rangedir start finish =
    if start < finish then 1
    else if start > finish then -1
    else failwith "unexpected"

let range1 start finish =
    if start <> finish then
        [ start .. (rangedir start finish) .. finish ]
    else
        failwith "unexpected"

let solve1 (lines: string []) =
    let numbers =
        lines
        |> Array.map readNumber
        |> Array.mapi (fun i x -> (i, x))
        |> Map.ofArray

    //print numbers
    //print numbers.Values
    let pos = Array.init numbers.Count (fun x -> x)
    let el = Array.init numbers.Count (fun x -> x)

    let realpos = circularPos numbers.Count

    //let find pos vp =
    //    pos |> Array.findIndex (fun e -> e = vp)
    let findPos vp =
        //pos |> Array.findIndex (fun e -> e = vp)
        el[vp]

    let setPos idx v =
        pos[idx] <- v
        el[v] <- idx

    let status =
        fun () ->
            [ 0 .. pos.Length - 1 ]
            |> List.map (fun i -> findPos i)
            |> List.map (fun idx -> numbers.Item idx)
            |> List.toArray

    for i in 0 .. numbers.Count - 1 do
        let offset = numbers.Item i % numbers.Count
        printm "process" (i, offset)

        if offset <> 0 then

            printm "offset" (i, offset)
            let startPos = pos[i]
            let finishPos = startPos + offset

            for step in (range1 startPos finishPos) do
                if step <> startPos then
                    let vstep = realpos step
                    let idx = findPos vstep
                    setPos idx (realpos (pos[idx] - rangedir startPos finishPos))

            setPos i (realpos finishPos)

    //printm "newpos" pos
    //printm "newstatus!" (status ())


    let zeroIndx = numbers |> Map.findKey (fun k v -> v = 0)
    let zeroIndxs = numbers |> Map.filter (fun k v -> v = 0)
    print zeroIndxs
    let zeroPos = pos[zeroIndx]

    let p1 = findPos (realpos (zeroPos + 1000))
    let c1 = numbers.Item p1
    print (p1, c1)

    let p2 = findPos (realpos (zeroPos + 2000))
    let c2 = numbers.Item p2
    print (p2, c2)

    let p3 = findPos (realpos (zeroPos + 3000))
    let c3 = numbers.Item p3
    print (p3, c3)
    let sln = c1 + c2 + c3

    //print numbers
    //print el
    print sln
    sln

let solve2 = solve1
