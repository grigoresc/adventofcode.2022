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

let readDigits (line: string) = readTokens line @"([^\-\d]+)" //todo wont work everytime that dash parsing..

let readNumber (line: string) = (readTokens line @"[^\d]+")[0]

let readMatrixOfDigits (lines: string []) =
    let v = lines |> Array.map readDigits
    v
