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
    | line when line.Contains "old * old" -> (fun x commonmult -> (x % commonmult) * (x % commonmult))
    | line when line.Contains "*" ->
        (fun x commonmult ->
            (x % commonmult)
            * (bigint (readNumber line) % commonmult))
    | line when line.Contains "+" -> (fun x commonmult -> x + (bigint (readNumber line)))
    | _ -> failwith "cannot parse"

let parseMonkey (lines: string list) =
    { originalItems = readNumbers lines[1]
      op = parseOp lines[2]
      div = bigint (readNumber lines[3])
      iftrue = readNumber lines[4]
      iffalse = readNumber lines[5] }

let parseInput lines =
    lines
    |> splitByCond ((=) "")
    |> List.map parseMonkey

let fct1 commonmult (monkey: Monkey) worryLevel : bigint =
    let multiply = monkey.op worryLevel commonmult
    bigint (System.Math.Floor(decimal (multiply) / 3m))

let fct2 commonmult (monkey: Monkey) worryLevel : bigint = monkey.op worryLevel commonmult

let inspect fct commonMult (monkey: Monkey) worryLevel : int * bigint =
    let bored = fct commonMult monkey worryLevel

    if bored % (monkey.div) = 0I then
        monkey.iftrue, bored
    else
        monkey.iffalse, bored

let solve fct rounds (monkeys: Monkey list) =
    let commonMult =
        monkeys
        |> Seq.map (fun x -> x.div)
        |> Seq.reduce (fun x y -> x * y)

    let monkeyItems =
        monkeys
        |> Seq.map (fun x -> List.ofArray (x.originalItems |> Array.map bigint))
        |> Seq.toArray

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
