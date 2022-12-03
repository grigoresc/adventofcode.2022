module aoc.day03.test

open aoc.day03.solutions
open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<Fact>]
let ``Solve day 3 sample`` () =
    let res = run solve1 "sample.txt"
    Assert.Equal(157, res)

[<Fact>]
let ``Solve day 3 input`` () =
    let res = run solve1 "input.txt"
    Assert.Equal(7766, res)

[<Fact>]
let ``Solve day 3 sample part2`` () =
    let res = run solve2 "sample.txt"
    Assert.Equal(70, res)

[<Fact>]
let ``Solve day 3 input part2`` () =
    let res = run solve2 "input.txt"
    Assert.Equal(2415, res)
