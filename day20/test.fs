module aoc.day20.test

open aoc.day20.solutions
open aoc.common

open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<Fact>]
let ``Solve day 20 sample`` () =
    let res = run solve1 "sample.txt"
    Assert.Equal(3, res)

[<Fact>]
[<NCrunch.Framework.Timeout(600000)>]
let ``Solve day 20 input`` () =
    let res = run solve1 "input.txt"
    Assert.Equal(2203, res)

[<Fact>]
let ``Solve day 20 sample part2`` () =
    let res = run solve2 "sample.txt"
    Assert.Equal(4, res)

[<Fact>]
let ``Solve day 20 input part2`` () =
    let res = run solve2 "input.txt"
    Assert.Equal(3, res)
