module aoc.day11.test

open aoc.day11.solutions
open aoc.common

open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver
let run2 solver txt = solver txt

[<Fact>]
let ``Solve day 11 input`` () =
    let res = run solve1 "input.txt"
    Assert.True(90882I = res)

[<Fact>]
let ``Solve day 11 sample`` () =
    let res = run solve1 "sample.txt"
    Assert.True(10605I = res)

[<Fact>]
let ``Solve day 11 input part2`` () =
    let res = run solve2 "input.txt"
    Assert.True(30893109657I = res)

[<Fact>]
let ``Solve day 11 sample part2`` () =
    let res = run solve2 "sample.txt"
    Assert.True(2713310158I = res)
