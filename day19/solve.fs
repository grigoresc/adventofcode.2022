module aoc.day19.solutions

open System.Text.RegularExpressions
open aoc.common
open System.Collections.Generic

[<Measure>]
type r

[<Measure>]
type l

[<Measure>]
type b

[<Measure>]
type geo


type Blueprint =
    { oreRobot: int<r>
      clayRobot: int<r>
      obsidianRobot: int<r>
      obsidianRobotClays: int<l>
      geocodeRobot: int<r>
      geocodeRobotObsidian: int<b> }

type State =
    { ores: int<r>
      clays: int<l>
      obs: int<b>
      rRobots: int
      lRobots: int
      bRobots: int
      gRobots: int
      time: int }
    static member defa =
        { ores = 0<r>
          clays = 0<l>
          obs = 0<b>
          rRobots = 1
          lRobots = 0
          bRobots = 0
          gRobots = 0
          time = 0 }

    member this.toString =
        $"state: {this.ores}/{this.clays}/{this.obs} {this.rRobots}/{this.lRobots}/{this.bRobots}/{this.gRobots} time={this.time}"

type Op =
    | BuildR
    | BuildL
    | BuildB
    | BuildG
    | Wait

type Command = { op: Op }
//type Command =
//    { tunnel: Tunnel
//      availableTime: int
//      instruction: Op }
//    member this.nextAvailableTime =
//        (this.availableTime
//         - 1
//         - if this.instruction = Op.Open then
//               1
//           else
//               0)

//    member this.addedPressure(valves: Map<string, Valve>) =
//        if this.instruction = Op.Open then
//            (this.availableTime - 1)
//            * (valves[this.tunnel.src].rate)
//        else
//            0
type Result = { cmd: Command; generated: int<geo> }
//type Result =
//    { command: Command
//      operated: bool
//      cost: int }
//    member this.tostring =
//        $"{this.command.tunnel.toString} {this.command.instruction} {this.operated} {this.command.availableTime} {this.cost}"

//let getNextState (state: State) (cmd: Command) =
//    let openValvesNext =
//        if cmd.instruction = Op.Open then
//            (cmd.tunnel.src :: state.openValves) |> List.sort
//        else
//            state.openValves

//    { openValves = openValvesNext }

//let canGoToNexState (nextState: State) (nextCmd: Command) (valves: Map<string, Valve>) =
//    if nextCmd.instruction = Op.Skip then
//        true
//    else if
//        valves[nextCmd.tunnel.src].rate > 0 //only it has some pressure
//        && not (List.contains nextCmd.tunnel.src nextState.openValves) //and only it isnt already open
//    then
//        true
//    else
//        false

let calcCost (b: Blueprint) (totalTime: int) : Result =
    let memoDict = Dictionary<_, _>()

    let maxR =
        List.max (
            [ b.oreRobot
              b.clayRobot
              b.obsidianRobot
              b.geocodeRobot ]
        )

    let maxL = List.max ([ b.obsidianRobotClays ])
    let maxB = List.max ([ b.geocodeRobotObsidian ])

    //cmd - we enter this method knowing it's possible to build something!!
    let rec calcMaxCost (state: State, cmd: Command) : Option<Result> =
        //printmPadded (totalTime - cmd.availableTime) $"{cmd.availableTime}/{cmd.instruction} tunnel" cmd.tunnel.toString

        if state.time = 0 then
            let result = { generated = 0<geo>; cmd = cmd }
            print state.toString
            //print state
            Some(result)
        else
            let producedGeo = state.rRobots * 1<geo>
            let producedOre = state.rRobots * 1<r>
            let producedClay = state.lRobots * 1<l>
            let producedObs = state.bRobots * 1<b>

            let newState =
                match cmd.op with
                | Op.BuildR -> { state with rRobots = state.rRobots + 1 }
                | Op.BuildL -> { state with lRobots = state.lRobots + 1 }
                | Op.BuildB -> { state with bRobots = state.bRobots + 1 }
                | Op.BuildG -> { state with gRobots = state.gRobots + 1 }
                | Op.Wait -> state


            let newState =
                { newState with
                    ores =
                        if newState.rRobots = 0 then
                            0<r>
                        else
                            (newState.ores + producedOre) % (maxR * newState.time * newState.rRobots)
                    clays =
                        if newState.lRobots = 0 then
                            0<l>
                        else
                            (newState.clays + producedClay) % (maxL * newState.time * newState.lRobots)
                    obs =
                        if newState.bRobots = 0 then
                            0<b>
                        else
                            (newState.obs + producedObs) % (maxB * newState.time * newState.bRobots) }

            let newState = { newState with time = newState.time - 1 }

            let mutable enoughCapacity = false

            let results =
                [ for nextOp in
                      [ Op.BuildG
                        Op.BuildB
                        Op.BuildL
                        Op.BuildR ] do
                      //try construct
                      let canBuild =
                          match nextOp with
                          | Op.BuildR -> b.oreRobot <= state.ores
                          | Op.BuildL -> b.clayRobot <= state.ores
                          | Op.BuildB ->
                              b.obsidianRobot <= state.ores
                              && b.obsidianRobotClays <= state.clays
                          | Op.BuildG ->
                              b.geocodeRobot <= state.ores
                              && b.geocodeRobotObsidian <= state.obs
                          | Op.Wait -> failwith "shouldnt be here.."

                      if canBuild then
                          enoughCapacity <- true
                          (memoize memoDict calcMaxCost (newState, { op = nextOp }))
                      else
                          None ]

            if not enoughCapacity then
                let resultAfterWait = (memoize memoDict calcMaxCost (newState, { op = Op.Wait }))

                resultAfterWait

            else
                let foundResults =
                    results
                    |> Seq.filter (fun x -> not (x = None))
                    |> Seq.map (fun x -> x.Value.generated)

                if Seq.isEmpty foundResults then
                    None
                else
                    let maxResult = Seq.max foundResults
                    print producedGeo

                    Some(
                        { generated = producedGeo + maxResult
                          cmd = cmd }
                    )
    //ii cmd.op=Op.Wait then
    //{ cost = 0<geo>; command = cmd }
    //let dest = cmd.tunnel.dest
    //let destValve = valves[dest]

    //let mutable maxCost = -1
    //let mutable maxCostChosen = None

    //let nextState = getNextState state cmd

    ////iterate through all possible tunnels
    //for destNext in destValve.valves do

    //    let tunnelNext = { src = dest; dest = destNext }

    //    //we have two cases
    //    //1. not open the next valve
    //    //2. open the next valve
    //    for instr in [ Op.Skip; Op.Open ] do
    //        let nextCmd =
    //            { tunnel = tunnelNext
    //              availableTime = cmd.nextAvailableTime
    //              instruction = instr }

    //        if canGoToNexState nextState nextCmd valves then
    //            let nextCost = (memoize memoDict calcMaxCost (nextState, nextCmd))

    //            if nextCost <> None then
    //                let totalCost =
    //                    nextCost.Value.Head.cost
    //                    + cmd.addedPressure (valves)

    //                if totalCost > maxCost then
    //                    maxCost <- totalCost
    //                    maxCostChosen <- Some(nextCost)

    ////printm $"{time}/{instructOpen} tunnel/tunelli" (tunnel.toString, maxCostChosen)
    ////printm $"{time}/{instructOpen} maxcost" maxCost

    //if maxCostChosen = None then
    //    None
    //else
    //    Some(
    //        { command = cmd
    //          operated = true
    //          cost = maxCost }
    //        :: maxCostChosen.Value.Value
    //    )

    let memoMaxCost = memoize memoDict calcMaxCost
    //let initialState = {S}
    let state = { State.defa with time = totalTime }
    let res = memoMaxCost (state, { op = Op.Wait }) //todo - we start for now with Op.Wait, surely we dont have resource yet

    printm "res for b" res
    res.Value

let parse (lines: string) =
    let ret = readDigits lines

    let b =
        { oreRobot = ret[1] * 1<r>
          clayRobot = ret[2] * 1<r>
          obsidianRobot = ret[3] * 1<r>
          obsidianRobotClays = ret[4] * 1<l>
          geocodeRobot = ret[5] * 1<r>
          geocodeRobotObsidian = ret[6] * 1<b> }

    //printm "b" b
    //|> L
    b


let solve1 (lines: string []) =
    let blueprints = lines |> Array.map parse //|> Map.ofSeq
    print blueprints
    //let sln = calcCost valves "AA" 30
    let b1 = calcCost blueprints[0] 24
    print b1
    //sln
    1

let solve2 = solve1
