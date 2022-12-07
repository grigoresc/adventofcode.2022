module aoc.day07.test

open aoc.day07.solutions
open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver
let run2 solver txt = solver txt


[<Fact>]
let ``Solve day 7 input`` () =
    let res = run solve1 "input.txt"
    Assert.Equal(1390824, res)

[<Fact>]
let ``Solve day 7 sample`` () =
    let res = run solve1 "sample.txt"
    Assert.Equal(95437, res)

[<Fact>]
let ``Solve day 7 input part2`` () =
    let res = run solve2 "input.txt"
    Assert.Equal(7490863, res)

[<Fact>]
let ``Solve day 7 sample part2`` () =
    let res = run solve2 "sample.txt"
    Assert.Equal(24933642, res)
