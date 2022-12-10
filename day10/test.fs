module aoc.day10.test

open aoc.day10.solutions
open aoc.common

open System.IO
open Xunit

let read x =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "//" + x)

let run solver filename = read filename |> solver
let run2 solver txt = solver txt


[<Fact>]
let ``Solve day 10 input`` () =
    let res = run solve1 "input.txt"
    Assert.Equal(11820, res)

[<Fact>]
let ``Solve day 10 sample`` () =
    let res = run solve1 "sample.txt"
    Assert.Equal(13140, res)

[<Fact>]
let ``Solve day 10 input part2`` () =
    let res = run solve2 "input.txt"

    let screen =
        [ "####.###....##.###..###..#..#..##..#..#."
          "#....#..#....#.#..#.#..#.#.#..#..#.#..#."
          "###..#..#....#.###..#..#.##...#..#.####."
          "#....###.....#.#..#.###..#.#..####.#..#."
          "#....#....#..#.#..#.#.#..#.#..#..#.#..#."
          "####.#.....##..###..#..#.#..#.#..#.#..#." ]

    Assert.True(screen.Equals(toStrings res)) //EPJBRKAH

[<Fact>]
let ``Solve day 10 sample part2`` () =
    let res = run solve2 "sample.txt"

    let screen =
        [ "##..##..##..##..##..##..##..##..##..##.."
          "###...###...###...###...###...###...###."
          "####....####....####....####....####...."
          "#####.....#####.....#####.....#####....."
          "######......######......######......####"
          "#######.......#######.......#######....." ]

    Assert.True(screen.Equals(toStrings res))
