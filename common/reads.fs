[<AutoOpen>]
module aoc.common.reads

open System.Text.RegularExpressions

let readTokens (line: string) splitpattern =
    let v =
        Regex
            .Replace(line, splitpattern, " ")
            .Trim()
            .Split(' ')
    //|> Array.map int

    v

let readDigits (line: string) =
    readTokens line @"[^\d]+" |> Array.map int

let readNonDigits (line: string) = readTokens line @"[\d]+"

let readNumber (line: string) =
    ((readTokens line @"[^\-\d]+") |> Array.map int)[0]

let readMatrixOfDigits (lines: string []) =
    let v = lines |> Array.map readDigits
    v
