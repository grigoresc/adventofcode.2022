module aoc.day08.test

open aoc.day08.solutions
open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver
let run2 solver txt = solver txt


[<Fact>]
let ``Solve day 8 input`` () =
    let res = run solve1 "input.txt"
    Assert.Equal(1805, res)

[<Fact>]
let ``Solve day 8 sample`` () =
    let res = run solve1 "sample.txt"
    Assert.Equal(21, res)

[<Fact>]
let ``Solve day 8 input part2`` () =
    let res = run solve2 "input.txt"
    Assert.Equal(444528, res)

[<Fact>]
let ``Solve day 8 sample part2`` () =
    let res = run solve2 "sample.txt"
    Assert.Equal(8, res)
