[<AutoOpen>]
module aoc.common.memoize

open System.Collections.Generic

let memoize (memoDict: Dictionary<_, _>) f =
    fun c ->
        let exist, value = memoDict.TryGetValue c
        //printm "c" c

        match exist with
        | true -> value
        | _ ->
            let value = f c
            //printm "add" c
            memoDict.Add(c, value)
            value

/// usage sample
/// let run = fib_sln 20 |> printfn "%i"
let fibonacci_sample (n: int) : int =
    let memoDict = Dictionary<_, _>()

    let rec fib (n: int) : int =
        match n with
        | 0
        | 1 -> n
        | n ->
            memoize memoDict fib (n - 1)
            + memoize memoDict fib (n - 2)

    let memoFib = memoize memoDict fib
    memoFib n
