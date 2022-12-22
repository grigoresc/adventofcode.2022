module aoc.day22.test

open aoc.day22.solutions
open aoc.common

open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<Fact>]
let ``Solve day 22 sample`` () =
    let res = run solve1 "sample.txt"
    Assert.Equal(6032, res)

[<Fact>]
let ``Solve day 22 input`` () =
    let res = run solve1 "input.txt"
    Assert.Equal(109094, res)

[<Fact>]
let ``Solve day 22 sample part2`` () =
    let res = run (solve2 QuizType.Sample) "sample.txt"
    Assert.Equal(5031, res)

[<Fact>]
let ``Solve day 22 input part2`` () =
    let res = run (solve2 QuizType.Input) "input.txt"
    Assert.Equal(53324, res)
