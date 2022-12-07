module aoc.day07.solutions

open aoc.common

let ok (s: string) = Set(s).Count = s.Length

let solve size (lines: string []) =
    let line = lines[0]

    let markers =
        seq {
            for i in [ 0 .. line.Length - size + 1 ] do
                if ok (line[i + 0 .. i + size - 1]) then
                    yield (i + size)
        }

    Seq.head markers

let rec process (dir: string) (ilst: string []) (acc: int list) =

    let mutable lst = ilst

    //printm "process dir" dir

    let cmd = lst[0]
    assert (cmd = "$ cd " + dir)

    let ls = lst[1]
    //print (cmd, ls)
    //print (lst[2..])
    let childs =
        (lst[2..])
        |> Array.takeWhile (fun x -> x[0] <> '$')

    //print childs
    let dirs = childs |> Array.where (fun x -> x[0] = 'd')
    //print dirs

    let files =
        childs
        |> Array.where (fun x -> x[0] <> 'd')
        |> Array.map (fun x ->
            let da = readDigits x
            da[0])

    let mutable sumfiles = Array.sum files
    //printm "files" (files, sumfiles)

    lst <- lst[2 + childs.Length ..]

    let mutable sumfiles1 = 0
    let mutable acc1 = acc

    for d in dirs do
        //while lst.Length > 0 do
        let (chilst, chisum, chisum1, chiacc) = process d[4..] lst acc
        sumfiles <- sumfiles + chisum
        acc1 <- chisum :: acc1
        acc1 <- chiacc @ acc1
        sumfiles1 <- sumfiles1 + chisum1

        if chisum <= 100000 then
            sumfiles1 <- sumfiles1 + chisum

        lst <- chilst

    //print lst
    //seq {for d in dirs do
    //    process lst
    //}
    //printm $"total for {dir}" (sumfiles, sumfiles1)
    (lst[1..], sumfiles, sumfiles1, acc1)

let solve1 (lines: string []) =
    let (lst, chisum, chisum1, _) = process "/" lines []
    print (lst, chisum, chisum1)

    chisum1

let solve2 (lines: string []) =
    let (lst, chisum, chisum1, acc) = process "/" lines []
    print (lst, chisum, chisum1, acc)

    let spaceneeded = chisum - (70000000 - 30000000)
    printm "space needed" spaceneeded
    printm "acc" acc
    let o = acc |> List.sort
    let x = o |> List.find (fun e -> e > spaceneeded)
    print (Array.ofList o)
    print x
    x
