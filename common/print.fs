[<AutoOpen>]
module aoc.common.out

let print x = printfn "%A" x
let printm msg x = printfn "%s>%A" msg x
