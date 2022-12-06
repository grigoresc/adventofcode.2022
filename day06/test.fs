module aoc.day06.test

open aoc.day06.solutions
open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver
let run2 solver txt = solver txt

[<Fact>]
let ``Solve day 6 sample`` () =
    let res = run2 solve1 [| "mjqjpqmgbljsphdztnvjfqwrcgsmlb" |]
    Assert.Equal(7, res)

[<Fact>]
let ``Solve day 6 sample2`` () =
    let res = run2 solve1 [| "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" |]
    Assert.Equal(11, res)

[<Fact>]
let ``Solve day 6 sample3`` () =
    let res = run2 solve1 [| "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" |]
    Assert.Equal(10, res)

[<Fact>]
let ``Solve day 6 input`` () =
    let res = run solve1 "input.txt"
    Assert.Equal(1912, res)

[<Fact>]
let ``Solve day 6 sample2 part2`` () =
    let res = run2 solve2 [| "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" |]
    Assert.Equal(26, res)

[<Fact>]
let ``Solve day 6 sample3 part2`` () =
    let res = run2 solve2 [| "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" |]
    Assert.Equal(29, res)

[<Fact>]
let ``Solve day 6 sample part2`` () =
    let res = run solve2 "sample.txt"
    Assert.Equal(19, res)

[<Fact>]
let ``Solve day 6 input part2`` () =
    let res = run solve2 "input.txt"
    Assert.Equal(2122, res)
