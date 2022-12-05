module aoc.day05.test

open aoc.day05.solutions
open System.IO
open Xunit
open aoc.common

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver

[<Fact>]
let ``Solve day 5 sample`` () =
    let stacksSample = [| "ZN"; "MCD"; "P" |]
    let res = run (solve1 stacksSample) "sample.txt"
    Assert.Equal("CMZ", res)

[<Fact>]
let ``Solve day 5 input`` () =
    let stacksInput =
        [| "WDGBHRV"
           "JNGCRF"
           "LSFHDNJ"
           "JDSV"
           "SHDRQWNM"
           "PGHCM"
           "FJBGLZHC"
           "SJR"
           "LGSRBNVM" |]

    let res = run (solve1 stacksInput) "input.txt"
    Assert.Equal("JRVNHHCSJ", res)

[<Fact>]
let ``Solve day 5 sample part2`` () =
    let stacksSample = [| "ZN"; "MCD"; "P" |]
    let res = run (solve2 stacksSample) "sample.txt"
    Assert.Equal("MCD", res)

[<Fact>]
let ``Solve day 5 input part2`` () =
    let stacksInput =
        [| "WDGBHRV"
           "JNGCRF"
           "LSFHDNJ"
           "JDSV"
           "SHDRQWNM"
           "PGHCM"
           "FJBGLZHC"
           "SJR"
           "LGSRBNVM" |]

    let res = run (solve2 stacksInput) "input.txt"
    Assert.Equal("GNFBSBJLH", res)
