module aoc.day22.run

open aoc.day22.solutions

open aoc.common
open System.IO

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<EntryPoint>]
let main args =
    let res = run (solve2 QuizType.Input) "day22/input.txt"
    print res
    0
