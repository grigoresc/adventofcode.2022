module aoc.day07.solutions

open aoc.common

let rec read (inList: string []) =

    let mutable next = inList

    let childs =
        (next[2..])
        |> Array.takeWhile (fun x -> x[0] <> '$')

    let dirs = childs |> Array.where (fun x -> x[0] = 'd')

    let files =
        childs
        |> Array.where (fun x -> x[0] <> 'd')
        |> Array.map readNumber

    let mutable totalSpace = Array.sum files
    next <- next[2 + childs.Length ..]
    let mutable acc = []

    for d in dirs do
        let (remained, childTotalSpace, childDirSpace) = read next
        totalSpace <- totalSpace + childTotalSpace
        acc <- childDirSpace @ acc
        next <- remained

    acc <- totalSpace :: acc
    (next[1..], totalSpace, acc)

let solve1 (lines: string []) =
    let (_, _, dirSpaces) = read lines

    dirSpaces |> List.where ((>=) 100000) |> List.sum


let solve2 (lines: string []) =
    let (_, totalSpace, dirSpaces) = read lines
    let spaceNeeded = totalSpace - (70000000 - 30000000)

    dirSpaces
    |> List.sort
    |> List.find ((<) spaceNeeded)
