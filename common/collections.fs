[<AutoOpen>]
module aoc.common.collections

let splitByCond predicate xs =
    [ let mutable lst = []

      for line in xs do
          match line with
          | line when predicate (line) = true ->
              yield lst
              lst <- []
          | _ -> lst <- lst @ [ line ]

      yield lst ]

/// flatmap on arrays
let ArrayCollect x =
    Seq.ofArray x |> Seq.collect id |> Seq.toArray

let toStrings (a: string [,]) : string list =
    [ for i in 0 .. Array2D.length1 a - 1 do
          yield String.concat "" a[i, *] ]
