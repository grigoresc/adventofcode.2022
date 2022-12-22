module aoc.day16.solutions

open System.Text.RegularExpressions
open aoc.common
open System.Collections.Generic

//Valve II has flow rate=0; tunnels lead to valves AA, JJ
let read (line: string) =
    let rx =

        Regex("^Valve (.{2}) has flow rate=([0-9]+); tunnels? lead to valve(s?) (.*)$", RegexOptions.Compiled)

    //print line
    let m = rx.Match(line)

    let (valve, rate, valves) =
        (m.Groups[1].Value), (m.Groups[2].Value), (m.Groups[ 4 ].Value.Split(", "))

    print (valve, rate, valves)
    line

    let run = memoFib 122 |> printfn "%i"

let solve1 (lines: string []) =
    let l = lines |> Array.map read

    let sln = l.Length
    sln

let solve2 (lines: string []) =
    let valves = lines |> Seq.map read |> Map.ofSeq

    let sln = calcCost valves "AA" 26
    sln
