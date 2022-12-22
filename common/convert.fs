[<AutoOpen>]
module aoc.common.convert

let asTupleOf2<'T> (lst: 'T list) =
    if lst.Length > 2 then
        failwith "more then 2 items for a tuple of two!"

    (lst[0], lst[1])

let asTupleOf3<'T> (lst: 'T list) =
    if lst.Length > 3 then
        failwith "more then 2 items for a tuple of two!"

    (lst[0], lst[1],lst[2])
