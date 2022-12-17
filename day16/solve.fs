module aoc.day16.solutions

open System.Text.RegularExpressions
open aoc.common
open System.Collections.Generic

type Tunnel =
    { src: string
      dest: string }
    member this.toString = $"{this.src} {this.dest}"

type Valve =
    { id: string
      rate: int
      valves: string [] }

type State = { openValves: string list }

type Op =
    | Skip
    | Open

type Command =
    { tunnel: Tunnel
      availableTime: int
      instruction: Op }
    member this.nextAvailableTime =
        (this.availableTime
         - 1
         - if this.instruction = Op.Open then
               1
           else
               0)

    member this.addedPressure(valves: Map<string, Valve>) =
        if this.instruction = Op.Open then
            (this.availableTime - 1)
            * (valves[this.tunnel.src].rate)
        else
            0

type Result =
    { command: Command
      operated: bool
      cost: int }
    member this.tostring =
        $"{this.command.tunnel.toString} {this.command.instruction} {this.operated} {this.command.availableTime} {this.cost}"

let read (line: string) =
    let m =
        Regex("^Valve (.{2}) has flow rate=([0-9]+); tunnels? leads? to valves? (.*)$", RegexOptions.Compiled)
            .Match(line)

    let (valve, rate, valves) =
        (m.Groups[1].Value), (m.Groups[2].Value), (m.Groups[ 3 ].Value.Split(", "))

    (valve,
     { id = valve
       rate = int (rate)
       valves = valves })

let getNextState (state: State) (cmd: Command) =
    let openValvesNext =
        if cmd.instruction = Op.Open then
            (cmd.tunnel.src :: state.openValves) |> List.sort
        else
            state.openValves

    { openValves = openValvesNext }

let canGoToNexState (nextState: State) (nextCmd: Command) (valves: Map<string, Valve>) =
    if nextCmd.instruction = Op.Skip then
        true
    else if
        valves[nextCmd.tunnel.src].rate > 0 //only it has some pressure
        && not (List.contains nextCmd.tunnel.src nextState.openValves) //and only it isnt already open
    then
        true
    else
        false

let calcCost (valves: Map<string, Valve>) (start: string) (totalTime: int) : int =
    let memoDict = Dictionary<_, _>()

    //calc max cost if we chose this tunnel, and have this time, and the source valve is going to be open or not
    //cost being the released pressure
    let rec calcMaxCost (state: State, cmd: Command) : Option<Result list> =
        //printmPadded (totalTime - cmd.availableTime) $"{cmd.availableTime}/{cmd.instruction} tunnel" cmd.tunnel.toString

        if List.contains cmd.availableTime [ 0..1 ] then //can't do anything in 0/1 minutes..
            Some(
                [ { command = cmd
                    operated = false
                    cost = 0 } ]
            )
        else
            let dest = cmd.tunnel.dest
            let destValve = valves[dest]

            let mutable maxCost = -1
            let mutable maxCostChosen = None

            let nextState = getNextState state cmd

            //iterate through all possible tunnels
            for destNext in destValve.valves do

                let tunnelNext = { src = dest; dest = destNext }

                //we have two cases
                //1. not open the next valve
                //2. open the next valve
                for instr in [ Op.Skip; Op.Open ] do
                    let nextCmd =
                        { tunnel = tunnelNext
                          availableTime = cmd.nextAvailableTime
                          instruction = instr }

                    if canGoToNexState nextState nextCmd valves then
                        let nextCost = (memoize memoDict calcMaxCost (nextState, nextCmd))

                        if nextCost <> None then
                            let totalCost =
                                nextCost.Value.Head.cost
                                + cmd.addedPressure (valves)

                            if totalCost > maxCost then
                                maxCost <- totalCost
                                maxCostChosen <- Some(nextCost)

            //printm $"{time}/{instructOpen} tunnel/tunelli" (tunnel.toString, maxCostChosen)
            //printm $"{time}/{instructOpen} maxcost" maxCost

            if maxCostChosen = None then
                None
            else
                Some(
                    { command = cmd
                      operated = true
                      cost = maxCost }
                    :: maxCostChosen.Value.Value
                )

    let memoMaxCost = memoize memoDict calcMaxCost

    let max =
        valves[start].valves
        |> Array.map (fun dest ->
            let cost =
                memoMaxCost (
                    { openValves = [] },
                    { tunnel = { src = start; dest = dest }
                      availableTime = totalTime
                      instruction = Op.Skip }
                )

            print cost
            cost.Value.Head.cost)

        |> Array.max

    max


let solve1 (lines: string []) =
    let valves = lines |> Seq.map read |> Map.ofSeq

    let sln = calcCost valves "AA" 30
    sln

let solve2 (lines: string []) =
    let valves = lines |> Seq.map read |> Map.ofSeq

    let sln = calcCost valves "AA" 26
    sln
