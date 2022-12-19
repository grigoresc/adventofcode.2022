module aoc.day17.run

open aoc.day17.solutions

open aoc.common
open System.IO

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<EntryPoint>]
let main args =
    //let res = run solve1 "input.txt"
    let res = run solve2 "input.txt"
    print res
    0
