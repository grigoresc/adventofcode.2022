[<AutoOpen>]
module aoc.common.out

let print x = printfn "%A" x
let printm msg x = printfn "%s>%A" msg x

let printmPadded n msg x =
    printfn "%s %s>%A" (atoString (Array.create n " ")) msg x

let printScreen<'T> (upsidedown: bool) (a: 'T [,]) =
    if upsidedown then
        let n = Array2D.length1 a

        a
        |> toStrings
        |> Seq.rev
        |> Seq.iteri (fun i x -> printfn "%3i |%s| %3i" (n - i - 1) x (n - i - 1))
    else
        a
        |> toStrings
        |> Seq.iteri (fun i x -> printfn "%3i |%s| %3i" i x i)
