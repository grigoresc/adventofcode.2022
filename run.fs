module aoc.day21.run

//open aoc.day21.solutions.sample
open aoc.day21.solutions.input

open aoc.common
open System.IO

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<EntryPoint>]
let main args =
    let res = solveInput
    print res
    0
