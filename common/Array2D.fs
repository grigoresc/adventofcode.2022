module Array2D

let findIndex<'T> (a: 'T array2d) f =

    let line =
        [ 0 .. Array2D.length2 a - 1 ]
        |> List.map (fun x -> (a[*, x]) |> Array.findIndex f)

    let col =
        [ 0 .. Array2D.length1 a - 1 ]
        |> List.map (fun x -> (a[x, *]) |> Array.findIndex f)

    (line, col)

let findIndexBack<'T> (a: 'T array2d) f =

    let line =
        [ 0 .. Array2D.length2 a - 1 ]
        |> List.map (fun x -> (a[*, x]) |> Array.findIndexBack f)

    let col =
        [ 0 .. Array2D.length1 a - 1 ]
        |> List.map (fun x -> (a[x, *]) |> Array.findIndexBack f)

    (line, col)

let lengths<'T> (a: 'T array2d) = Array2D.length1 a, Array2D.length2 a
