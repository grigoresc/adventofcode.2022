module aoc.day15.test

open aoc.day15.solutions
open aoc.common

open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<Fact>]
[<NCrunch.Framework.Timeout(600000)>]
let ``Solve day 15 input`` () =
    let res = run (solve1 2000000) "input.txt"
    Assert.Equal(779, res)

[<Fact>]
let ``Solve day 15 sample`` () =
    let res = run (solve1 10) "sample.txt"
    Assert.Equal(26, res)

[<Fact>]
let ``Solve day 15 input part2`` () =
    let res = run (solve2 2000000) "input.txt"
    Assert.Equal(27426, res)

[<Fact>]
let ``Solve day 15 sample part2`` () =
    let res = run (solve2 10) "sample.txt"
    Assert.Equal(26, res)
