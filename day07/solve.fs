module aoc.day07.solutions
open aoc.common

let rec browse (dir: string) (inList: string [])  =

    let mutable next = inList

    let childs =
        (next[2..])
        |> Array.takeWhile (fun x -> x[0] <> '$')

    let dirs = childs |> Array.where (fun x -> x[0] = 'd')

    let files =
        childs
        |> Array.where (fun x -> x[0] <> 'd')
        |> Array.map readNumber

    let mutable sum = Array.sum files
    next <- next[2 + childs.Length ..]
    let mutable acc = []

    for d in dirs do
        let (childNext, childSize, childAcc) = browse d[4..] next 
        sum <- sum + childSize
        acc <- childAcc @ acc
        next <- childNext 

    acc<-sum::acc
    (next[1..], sum,  acc)

let solve1 (lines: string []) =
    let (_, _,  acc) = browse "/" lines 
    let sumUnder1Mil = acc|>List.where (fun e->e<=100000)|>List.sum

    sumUnder1Mil

let solve2 (lines: string []) =
    let (_, chisum, acc) = browse "/" lines 

    let spaceNeeded = chisum - (70000000 - 30000000)
    let toDelete = acc |> List.sort|> List.find (fun e -> e > spaceNeeded)
    toDelete 

