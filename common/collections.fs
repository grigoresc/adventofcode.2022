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

let atoString<'T> (a: 'T []) : string =
    String.concat "" (a |> Seq.map (fun x -> x.ToString()))

let toStrings<'T> (a: 'T [,]) : string list =
    [ for i in 0 .. Array2D.length1 a - 1 do
          yield atoString a[i, *] ]

let array2d_findIndex<'T> (a: 'T array2d) f =

    let line =
        [ 0 .. Array2D.length2 a - 1 ]
        |> List.map (fun x -> (a[*, x]) |> Array.findIndex f)

    let col =
        [ 0 .. Array2D.length1 a - 1 ]
        |> List.map (fun x -> (a[x, *]) |> Array.findIndex f)

    (line, col)

let array2d_findIndexBack<'T> (a: 'T array2d) f =

    let line =
        [ 0 .. Array2D.length2 a - 1 ]
        |> List.map (fun x -> (a[*, x]) |> Array.findIndexBack f)

    let col =
        [ 0 .. Array2D.length1 a - 1 ]
        |> List.map (fun x -> (a[x, *]) |> Array.findIndexBack f)

    (line, col)
