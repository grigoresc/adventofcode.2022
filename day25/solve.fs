module aoc.day25.solutions

open aoc.common

let solve (lines: string []) =
    let numbers = lines

    let fromSnafuDigit ndigit =
        match ndigit with
        | '=' -> -2
        | '-' -> -1
        | '0' -> 0
        | '1' -> 1
        | '2' -> 2
        | _ -> failwith "not the case"

    let toSnafuDigit n =
        match n with
        | -2 -> '='
        | -1 -> '-'
        | 0 -> '0'
        | 1 -> '1'
        | 2 -> '2'
        | _ -> failwith "not the case"

    let toSnafu (n: int64) =
        let mutable c = n
        let mutable sc = ""

        while c > 0 do
            let d = int ((c + 2L) % 5L) - 2
            sc <- (toSnafuDigit d).ToString() + sc
            c <- c - int64 (d)
            c <- int64 (c / 5L)

        sc

    let fromSnafu (n: string) =
        let n10 =
            n.ToCharArray()
            |> Array.mapi (fun i d ->
                let snafuf =
                    (pown 5L (n.Length - i - 1))
                    * int64 (fromSnafuDigit d)

                snafuf)
            |> Array.sum

        printm $"from {n} to" n10
        printm "to snafu back" (toSnafu n10)
        n10

    let numbers10 = numbers |> Array.map fromSnafu
    let sum = numbers10 |> Array.sum
    let snafus = toSnafu sum
    snafus

let solve1 = solve
let solve2 = solve
