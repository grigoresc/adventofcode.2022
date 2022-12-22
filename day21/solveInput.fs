module aoc.day21.solutions.input
open day21.Input

let solveInput =
    let mutable expected = 0L

    let sln1 =
        root
            (fun a b ->
                expected <- b
                a + b)
            721L

    //print sln1
    //printm "ref" expected

    let calc x =
        let mutable actual = 0L

        let r =
            root
                (fun a b ->
                    actual <- a
                    a = b)
                x

        actual

    let newinterval (a, b) =
        let va = calc a
        let vb = calc b
        let middle = a + int64 ((b - a) / 2L)
        let vm = calc middle
        //print $"compare {a}/{middle}/{b} {va}/{vm}/{vb} {expected}"

        if va > expected && expected > vm then
            (a, middle)
        else if vm > expected && expected > vb then
            (middle, b)
        else if vm = expected then
            (middle, middle)
        else
            failwith $"unexpected case, possible out of range {a}/{middle}/{b} {va}/{vm}/{vb} {expected}"

    let mutable finish = false
    let mutable a = 0L
    let mutable b = 5100000000000L

    while not finish do
        let (na, nb) = newinterval (a, b)
        if na = nb then finish <- true
        a <- na
        b <- nb

    let sln2 = a
    //print sln2

    sln1, sln2
