module aoc.day21.test

open aoc.day21.solutions.sample
open aoc.day21.solutions.input
open aoc.common

open System.IO
open Xunit


[<Fact>]
let ``Solve day 21 sample part1/2`` () =
    let res = solveSample
    Assert.Equal((152, 301), res)


[<Fact>]
let ``Solve day 21 input part1/2`` () =
    let res = solveInput
    Assert.Equal((70674280581468L, 3243420789721L), res)

