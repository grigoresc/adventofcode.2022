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

/// flatmap on arrays
let toArray (arr: 'T [,]) = arr |> Seq.cast<'T> |> Seq.toArray

let atoString (a: string []) : string = String.concat "" a

let atoString2 (a: char []) : string = System.String.Concat a //todo why is it so complicated to transform char[] to string??

let toStrings (a: string [,]) : string list =
    [ for i in 0 .. Array2D.length1 a - 1 do
          yield atoString a[i, *] ]

let toStrings2 (a: char [,]) : string list =
    [ for i in 0 .. Array2D.length1 a - 1 do
          yield atoString2 (a[i, *]) ]
