module aoc.day14.test

open aoc.day14.solutions
open aoc.common

open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<Fact>]
[<NCrunch.Framework.Timeout(600000)>]
let ``Solve day 14 input`` () =
    let res = run solve1 "input.txt"
    Assert.True(504 = res)

[<Fact>]
let ``Solve day 14 sample`` () =
    let res = run solve1 "sample.txt"
    Assert.True(31 = res)

[<Fact>]
let ``Solve day 14 input part2`` () =
    let res = run solve2 "input.txt"
    Assert.True(500 = res)

[<Fact>]
let ``Solve day 14 sample part2`` () =
    let res = run solve2 "sample.txt"
    Assert.True(29 = res)
