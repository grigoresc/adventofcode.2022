module aoc.day01.test

open aoc.common
open aoc.day01.solutions
open System
open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<Fact>]
let ``Solve day 2`` () =
    let res = run solve "input.txt"
    Assert.Equal((74198, 209914), res)
