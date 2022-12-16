module aoc.day16.solutions

open System.Text.RegularExpressions

open aoc.common
open System.Collections.Generic

type Tunnel = { src: string; dest: string }

type TunnelCost =
    { src: string
      dest: string
      costsPerLevel: Map<int, int>
      constantFluxLevel: int }

type Valve =
    { id: string
      rate: int
      valves: string [] }

let memoize f =
    let dict = Dictionary<_, _>();
    fun c ->
        let exist, value = dict.TryGetValue c
        match exist with
        | true -> value
        | _ -> 
            let value = f c
            dict.Add(c, value)
            value

let rec fib(n: int):int = 
    match n with
    | 0 | 1 -> n
    | n -> fib (n-1) + fib (n - 2)

let calcCost (valves: Map<string, Valve>) (tunnel: Tunnel) (level: int) : int =

    let rec calcMinCost (tunnel: Tunnel) (level: int) : int =
        let dest = tunnel.dest
        let destValve = valves[dest]
        let mutable mincost = System.Int32.MaxValue
        let mutable minTunnel = None

        for desti in destValve.valves do

            let tunneli = { src = dest; dest = desti }
            let cost = calcMinCost tunneli (level + 1)

            if cost < mincost then
                mincost <- mincost
                minTunnel <- Some(tunneli)

        mincost

    calcMinCost tunnel level

let read (line: string) =
    let rx =
        Regex("^Valve (.{2}) has flow rate=([0-9]+); tunnels? leads? to valves? (.*)$", RegexOptions.Compiled)

    //print line
    let m = rx.Match(line)

    let (valve, rate, valves) =
        (m.Groups[1].Value), (m.Groups[2].Value), (m.Groups[ 3 ].Value.Split(", "))

    (valve,
     { id = valve
       rate = int (rate)
       valves = valves })

let solve1 (lines: string []) =
    let memoFib = memoize fib

    let run = memoFib 122 |> printfn "%i"

    let valves = lines |> Seq.map read |> Map.ofSeq
    printm "v" valves

    let sln = valves.Count
    sln

let solve2 = solve1
