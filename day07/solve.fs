module aoc.day07.solutions

open aoc.common

let rec browse (dir: string) (ilst: string []) (acc: int list) =

    let mutable lst = ilst

    let cmd = lst[0]
    assert (cmd = "$ cd " + dir)

    let ls = lst[1]
    let childs =
        (lst[2..])
        |> Array.takeWhile (fun x -> x[0] <> '$')

    let dirs = childs |> Array.where (fun x -> x[0] = 'd')

    let files =
        childs
        |> Array.where (fun x -> x[0] <> 'd')
        |> Array.map readNumber

    let mutable sumfiles = Array.sum files

    lst <- lst[2 + childs.Length ..]

    let mutable acc1 = acc

    for d in dirs do
        let (chilst, chisum, chiacc) = browse d[4..] lst acc
        sumfiles <- sumfiles + chisum
        acc1 <- chiacc @ acc1
        lst <- chilst

    acc1<-sumfiles::acc1
    (lst[1..], sumfiles,  acc1)

let solve1 (lines: string []) =
    let (_, _,  acc) = browse "/" lines []
    let sumUnder1Mil = acc|>List.where (fun e->e<=100000)|>List.sum

    sumUnder1Mil

let solve2 (lines: string []) =
    let (_, chisum, acc) = browse "/" lines []

    let spaceneeded = chisum - (70000000 - 30000000)
    let delete = acc |> List.sort|> List.find (fun e -> e > spaceneeded)
    delete 
