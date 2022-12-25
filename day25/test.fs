module aoc.day25.test

open aoc.day25.solutions
open aoc.common

open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<Fact>]
let ``Solve day 25 sample`` () =
    let res = run solve1 "sample.txt"
    Assert.Equal("2=-1=0", res)

[<Fact>]
let ``Solve day 25 input`` () =
    let res = run solve1 "input.txt"
    Assert.Equal("2-==10--=-0101==1201", res)

[<Fact>]
let ``Solve day 25 sample part2`` () =
    let res = run solve2 "sample.txt"
    Assert.Equal("", res)

[<Fact>]
[<NCrunch.Framework.Timeout(600000)>]
let ``Solve day 25 input part2`` () =
    let res = run solve2 "input.txt"
    Assert.Equal("2=-1=0", res)
