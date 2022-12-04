module aoc.day04.test

open aoc.day04.solutions
open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<Fact>]
let ``Solve day 4 sample`` () =
    let res = run solve1 "sample.txt"
    Assert.Equal(2, res)

[<Fact>]
let ``Solve day 4 input`` () =
    let res = run solve1 "input.txt"
    Assert.Equal(453, res)

[<Fact>]
let ``Solve day 4 sample part2`` () =
    let res = run solve2 "sample.txt"
    Assert.Equal(4, res)

[<Fact>]
let ``Solve day 4 input part2`` () =
    let res = run solve2 "input.txt"
    Assert.Equal(919, res)
