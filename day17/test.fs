module aoc.day17.test

open aoc.day17.solutions
open aoc.common

open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<Fact>]
let ``Solve day 17 sample`` () =
    let res = run solve1 "sample.txt"
    Assert.Equal(3068, res)

[<Fact>]
[<NCrunch.Framework.Timeout(600000)>]
let ``Solve day 17 input`` () =
    let res = run solve1 "input.txt"
    Assert.Equal(3161, res)

[<Fact>]
let ``Solve day 17 sample part2`` () =
    let res = run solve2 "sample.txt"
    Assert.Equal(4, res)

[<Fact>]
[<NCrunch.Framework.Timeout(600000)>]
let ``Solve day 17 input part2`` () =
    let res = run solve2 "input.txt"
    Assert.Equal(3, res)
