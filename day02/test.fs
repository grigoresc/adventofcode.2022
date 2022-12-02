module aoc.day02.test

open aoc.day02.solutions
open System
open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<Fact>]
let ``Solve day 2`` () =
    let res = run solve "input.txt"
    Assert.Equal((12156, 10835), res)
