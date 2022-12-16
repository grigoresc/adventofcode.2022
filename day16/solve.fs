module aoc.day16.solutions

open System.Text.RegularExpressions

open aoc.common

open System.Collections.Generic

type Tunnel =
    { src: string
      dest: string }
    member this.toString = this.src + " " + this.dest

type TunnelCost =
    { src: string
      dest: string
      costsPerLevel: Map<int, int>
      constantFluxLevel: int }

type Valve =
    { id: string
      rate: int
      valves: string [] }

let memoize (memoDict: Dictionary<_, _>) f =
    fun c ->
        let exist, value = memoDict.TryGetValue c
        //printm "c" c

        match exist with
        | true -> value
        | _ ->
            let value = f c
            //printm "add" c
            memoDict.Add(c, value)
            value


let fib_sln (n: int) : int =
    let memoDict = Dictionary<_, _>()

    let rec fib (n: int) : int =
        match n with
        | 0
        | 1 -> n
        | n ->
            memoize memoDict fib (n - 1)
            + memoize memoDict fib (n - 2)

    let memoFib = memoize memoDict fib
    memoFib n

type Result =
    { tunnel: Tunnel
      openInstr: bool
      operated: bool
      minute: int
      cost: int }
    member this.tostring =
        this.tunnel.toString
        + " "
        + this.openInstr.ToString()
        + " "
        + this.operated.ToString()
        + " "
        + this.minute.ToString()
        + " "
        + this.cost.ToString()

let calcCost (valves: Map<string, Valve>) (tunnel: Tunnel) : Option<Result list> =
    let memoDict = Dictionary<_, _>()

    let totalTime = 30
    //calc max cost if we chose this tunnel, and have this time, and the source valve is going to be open or not
    //cost being the released pressure
    let rec calcMaxCost (openvalves: string list, tunnel: Tunnel, time: int, instructOpen: bool) : Option<Result list> =
        //printmPadded (totalTime - time) $"{time}/{instructOpen} tunnel" tunnel.toString

        if time = 0 then
            //print "cant do anything with 0 time"

            Some(
                [ { tunnel = tunnel
                    openInstr = instructOpen
                    operated = false
                    minute = time
                    cost = 0 } ]
            )
        else if time = 1 then
            //print "cant do anything with 1 time; even if we may open the valve.."

            Some(
                [ { tunnel = tunnel
                    openInstr = instructOpen
                    operated = false
                    minute = time
                    cost = 0 } ]
            )
        else
            let src = tunnel.src
            let dest = tunnel.dest
            let destValve = valves[dest]

            let mutable maxCost = -1
            let mutable maxCostChosen = None

            let timeTaken = 1 + (if instructOpen then 1 else 0)

            let newopenvalves =
                if instructOpen then
                    (src :: openvalves) |> List.sort
                else
                    openvalves

            let extraPressure =
                if instructOpen then
                    (time - 1) * (valves[src].rate)
                else
                    0

            for desti in destValve.valves do

                let tunneli = { src = dest; dest = desti }

                //we have two cases
                //1. not going to open the next valve
                let c1 =
                    (memoize memoDict calcMaxCost (newopenvalves, tunneli, (time - timeTaken), false))

                if c1 <> None then
                    let alreadyopen1 =
                        c1.Value
                        |> List.tryFind (fun e ->
                            e.tunnel.src = src
                            && e.openInstr = true
                            && e.operated = true)

                    if instructOpen = false || alreadyopen1 = None then
                        let costNotOpenNext = extraPressure + c1.Value.Head.cost

                        if costNotOpenNext > maxCost then
                            maxCost <- costNotOpenNext
                            maxCostChosen <- Some(c1)

                //2. going to open the next valve
                if not (List.contains tunneli.src newopenvalves) then
                    let c2 =
                        (memoize memoDict calcMaxCost (newopenvalves, tunneli, (time - timeTaken), true))

                    if c2 <> None then
                        let alreadyopen2 =
                            c2.Value
                            |> List.tryFind (fun e ->
                                e.tunnel.src = src
                                && e.openInstr = true
                                && e.operated = true)

                        if instructOpen = false || alreadyopen2 = None then
                            let costOpenNext = extraPressure + c2.Value.Head.cost

                            if costOpenNext > maxCost then
                                maxCost <- costOpenNext
                                maxCostChosen <- Some(c2)

            //printm $"{time}/{instructOpen} tunnel/tunelli" (tunnel.toString, maxCostChosen)

            //printm $"{time}/{instructOpen} maxcost" maxCost

            if maxCostChosen = None then
                None
            else
                Some(
                    { tunnel = tunnel
                      openInstr = instructOpen
                      operated = true
                      minute = time
                      cost = maxCost }
                    :: maxCostChosen.Value.Value
                )

    //let memoMaxCost = memoize calcMaxCost

    let memoMaxCost = memoize memoDict calcMaxCost

    let ret = memoMaxCost ([], tunnel, totalTime, false)
    ret

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
    //let run = fib_sln 20 |> printfn "%i"

    let valves = lines |> Seq.map read |> Map.ofSeq
    //printm "valves" valves

    let res = calcCost valves { src = "AA"; dest = "DD" }
    //printm "res" res
    printm "res" (res.Value |> List.map (fun e -> e.tostring))

    let sln = valves.Count
    sln

let solve2 lines = 1
