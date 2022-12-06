[<AutoOpen>]
module aoc.common.reads

open System.Text.RegularExpressions

let readTokens (line: string) splitpattern =
    let v =
        Regex
            .Replace(line, splitpattern, " ")
            .Trim()
            .Split(' ')
        |> Array.map int

    v

let readDigits (line: string) = readTokens line @"[^\d]+"

let readMatrixOfDigits (lines: string []) =
    let v = lines |> Array.map readDigits
    v
