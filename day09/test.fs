module aoc.day09.test

open aoc.day09.solutions
open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver
let run2 solver txt = solver txt


[<Fact>]
let ``Solve day 9 input`` () =
    let res = run solve1 "input.txt"
    Assert.Equal(6284, res)

[<Fact>]
let ``Solve day 9 sample`` () =
    let res = run solve1 "sample.txt"
    Assert.Equal(13, res)

[<Fact>]
let ``Solve day 9 input part2`` () =
    let res = run solve2 "input.txt"
    Assert.Equal(2661, res)

[<Fact>]
let ``Solve day 9 sample part2`` () =
    let res = run solve2 "sample.txt"
    Assert.Equal(1, res)

[<Fact>]
let ``Solve day 9 sample2 part2`` () =
    let res = run solve2 "sample2.txt"
    Assert.Equal(36, res)
