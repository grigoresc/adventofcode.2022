module aoc.day16.test

open aoc.day16.solutions
open aoc.common

open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<Fact>]
[<NCrunch.Framework.Timeout(600000)>]
let ``Solve day 16 input`` () =
    let res = run solve1 "input.txt"
    Assert.Equal(779, res)

[<Fact>]
let ``Solve day 16 sample`` () =
    let res = run solve1 "sample.txt"
    Assert.Equal(24, res)

[<Fact>]
let ``Solve day 16 input part2`` () =
    let res = run solve2 "input.txt"
    Assert.Equal(27426, res)

[<Fact>]
let ``Solve day 16 sample part2`` () =
    let res = run solve2 "sample.txt"
    Assert.Equal(93, res)
