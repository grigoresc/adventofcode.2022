[<AutoOpen>]
module aoc.common.reads

open System.Text.RegularExpressions

let readTokens (line: string) splitpattern =
    Regex
        .Replace(line, splitpattern, " ")
        .Trim()
        .Split(' ')

let readNumbers (line: string) =
    readTokens line @"[^\-\d]+" |> Array.map int

let readNonNumbers (line: string) = readTokens line @"[\d]+"

let readNumber (line: string) =
    ((readTokens line @"[^\-\d]+") |> Array.map int)[0]

let readMatrixOfNumbers (lines: string []) = lines |> Array.map readNumbers
