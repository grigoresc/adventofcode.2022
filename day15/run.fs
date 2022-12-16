module aoc.day15.run

open aoc.day15.solutions

open aoc.common
open System.IO

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<EntryPoint>]
let main args =
    let res = run (solve1 2000000) "input.txt"
    //let res = run (solve1 10) "sample.txt"
    0
