module aoc.day11.solutions

open aoc.common

type Monkey =
    { originalItems: int array
      op: bigint -> bigint -> bigint
      div: bigint
      iftrue: int
      iffalse: int }

let parseOp (line: string) =
    match line with
    | line when line.Contains "old * old" -> (fun x commondiv -> (x % commondiv) * (x % commondiv))
    | line when line.Contains "*" ->
        (fun x commondiv ->
            (x % commondiv)
            * (bigint (readNumber (line)) % commondiv))
    | line when line.Contains "+" -> (fun x commondiv -> x + (bigint (readNumber (line))))
    | _ -> failwith "cannot parse"

let parseMonkey (lines: string list) =
    { originalItems = readDigits lines[1]
      op = parseOp lines[2]
      div = bigint (readNumber lines[3])
      iftrue = readNumber lines[4]
      iffalse = readNumber lines[5] }

let parseInput lines =
    lines
    |> splitByCond ((=) "")
    |> List.toArray
    |> Array.map parseMonkey

let fct1 commondiv (monkey: Monkey) worryLevel : bigint =
    let multiply = monkey.op worryLevel commondiv
    bigint (System.Math.Floor(decimal (multiply) / 3m))

let fct2 commondiv (monkey: Monkey) worryLevel : bigint = monkey.op worryLevel commondiv

let inspect fct commonMult (monkey: Monkey) worryLevel : int * bigint =
    let bored = fct commonMult monkey worryLevel

    if bored % (monkey.div) = 0I then
        monkey.iftrue, bored
    else
        monkey.iffalse, bored

let solve fct rounds (monkeys: Monkey array) =
    let commonMult =
        monkeys
        |> Seq.map (fun x -> x.div)
        |> Seq.reduce (fun x y -> x * y)

    let monkeyItems =
        monkeys
        |> Array.map (fun x -> List.ofArray (x.originalItems |> Array.map bigint))

    let mutable state = (monkeyItems, Array.create monkeys.Length 0)

    [ 1..rounds ]
    |> List.iter (fun r ->
        state <-
            (state, monkeyItems, [| 0 .. monkeys.Length - 1 |])
            |||> Array.fold2 (fun (accItems, accCnt) mItems mIdx ->

                let newm =
                    accItems[mIdx]
                    |> List.map (inspect fct commonMult monkeys[mIdx])

                accItems[mIdx] <- []

                newm
                |> List.iter (fun (next, level) ->
                    accItems[next] <- level :: accItems[next]
                    accCnt[mIdx] <- accCnt[mIdx] + 1)

                (accItems, accCnt)))

    let sln =
        snd (state)
        |> Seq.sortDescending
        |> Seq.map bigint
        |> Seq.take 2
        |> Seq.reduce (fun x y -> x * y)

    sln

let solve1 (lines: string []) = lines |> parseInput |> solve fct1 20
let solve2 (lines: string []) = lines |> parseInput |> solve fct2 10000
