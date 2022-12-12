module aoc.day12.test

open aoc.day12.solutions
open aoc.common

open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<Fact>]
[<NCrunch.Framework.Timeout(600000)>]
let ``Solve day 12 input`` () =
    let res = run solve1 "input.txt"
    Assert.True(504 = res)

[<Fact>]
let ``Solve day 12 sample`` () =
    let res = run solve1 "sample.txt"
    Assert.True(31 = res)

[<Fact>]
let ``Solve day 12 input part2`` () =
    let res = run solve2 "input.txt"
    Assert.True(500 = res)

[<Fact>]
let ``Solve day 12 sample part2`` () =
    let res = run solve2 "sample.txt"
    Assert.True(29 = res)
