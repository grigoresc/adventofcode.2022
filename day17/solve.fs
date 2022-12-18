module aoc.day17.solutions

open aoc.common

type Coord =
    { X: int
      Y: int }
    static member (+)(a: Coord, b: Coord) = { X = a.X + b.X; Y = a.Y + b.Y }

type Rock =
    { pattern: Set<Coord>
      pos: Coord
      idx: int }
    static member realcoord(rock: Rock) : Set<Coord> =
        rock.pattern |> Set.map (fun p -> p + rock.pos)

let MAXX = 6

let Floor =
    Set(
        [ { X = 0; Y = 0 }
          { X = 1; Y = 0 }
          { X = 2; Y = 0 }
          { X = 3; Y = 0 }
          { X = 4; Y = 0 }
          { X = 5; Y = 0 }
          { X = MAXX; Y = 0 } ] //todo - is it ok?
    )

let RockTypes =
    [ Set(
          [ { X = 0; Y = 0 }
            { X = 1; Y = 0 }
            { X = 2; Y = 0 }
            { X = 3; Y = 0 } ]
      )
      Set(
          [ { X = 0; Y = 1 }
            { X = 1; Y = 0 }
            { X = 1; Y = 1 }
            { X = 1; Y = 2 }
            { X = 2; Y = 1 } ]
      )
      Set(
          [ { X = 0; Y = 0 }
            { X = 1; Y = 0 }
            { X = 2; Y = 0 }
            { X = 2; Y = 1 }
            { X = 2; Y = 2 } ]
      )
      Set(
          [ { X = 0; Y = 0 }
            { X = 0; Y = 1 }
            { X = 0; Y = 2 }
            { X = 0; Y = 3 } ]
      )
      Set(
          [ { X = 0; Y = 0 }
            { X = 0; Y = 1 }
            { X = 1; Y = 0 }
            { X = 1; Y = 1 } ]
      ) ]

type Stack = { rocks: Rock list }

let printStack stack = printm "rocks" stack.rocks

let getAllPoints (stack: Stack) =
    //print stack

    //todo - any other option here to implement?
    let x =
        match stack with
        | stack when not (stack.rocks.IsEmpty) ->
            stack.rocks
            |> List.map (fun r -> r.pattern |> Set.map (fun p -> p + r.pos))
            |> List.reduce (fun a b -> a + b)
        | _ -> Set.empty

    //print x
    x

let drawStack (stack: Stack) =
    stack
    |> getAllPoints
    |> Set.map (fun p -> (p.X, p.Y))
    |> printScreenM17
        { minX = 0
          minY = 0
          maxX = MAXX
          maxY = 10 }
        true

let drawStack2 (points: Set<Coord>) =
    points
    |> Set.map (fun p -> (p.X, p.Y))
    |> printScreenM17
        { minX = 0
          minY = 0
          maxX = MAXX
          maxY = 10 }
        true

let add rock stack = { rocks = rock :: stack.rocks }

let safePrepend<'T> x (list: 'T list) = //todo why do I need to write this function, isnt something builtin?
    if not list.IsEmpty then
        x :: list
    else
        [ x ]

let getHeight filled =
    let maxY = filled |> Set.map (fun e -> e.Y) |> Set.maxElement
    maxY

let WINDOW = 130
//todo - why seq where on jet takes so much time to get the top (5ms?)
let processRock (jet: string, idx: int (*state: Stack, *) , filled: Set<Coord>) (rock: Rock) =
    //drawStack2 filled

    let filled =
        Set(
            filled
            |> Set.toList
            |> List.sortByDescending (fun f -> f.Y)
            |> List.take (min WINDOW filled.Count)
        )


    let maxY = filled |> Set.map (fun e -> e.Y) |> Set.maxElement

    if rock.idx % 50 = 0 then
        printm $"{rock.idx}" (maxY, filled.Count)
    //print filled
    //drawStack2 filled
    //drawStack2 filledprev
    //let maxY = getHeight state
    //let filled = getAllPoints state

    let mutable anotherMoveAllowed = true

    let mutable therock = { rock with pos = { X = 2; Y = maxY + 4 } }
    //drawStack { state with rocks = safePrepend therock state.rocks }

    let mutable cnt = 0

    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let mutable currentIdx = idx

    while anotherMoveAllowed do
        currentIdx <- currentIdx + 1
        let nextCmd = jet[currentIdx % jet.Length]
        //let nextCmd = Seq.head currentJetStream
        //printfn "  Elapsed Time 0: %i" timer.ElapsedMilliseconds
        cnt <- cnt + 1
        //printm "cmd" nextCmd

        //printfn "  Elapsed Time 1: %i" timer.ElapsedMilliseconds

        let nextPos =
            match nextCmd with
            | '<' ->
                { X = (therock.pos.X - 1)
                  Y = therock.pos.Y }
            | '>' ->
                { X = (therock.pos.X + 1)
                  Y = therock.pos.Y }
            | _ -> failwith "not possible"

        let rockFill = Rock.realcoord { therock with pos = nextPos }

        let throughthewall (rockFill: Set<Coord>) =
            //todo - move also the floor here..
            rockFill
            |> Set.exists (fun c -> c.X < 0 || c.X > MAXX)

        if
            not (throughthewall rockFill)
            && Set.intersect rockFill filled = Set.empty
        then
            therock <- { therock with pos = nextPos }

        //printfn "  Elapsed Time 2: %i" timer.ElapsedMilliseconds

        //drawStack { state with rocks = safePrepend therock state.rocks }

        let downPos = { therock.pos with Y = (therock.pos.Y - 1) }
        let rockFillDown = Rock.realcoord { therock with pos = downPos }

        if
            not (throughthewall rockFillDown)
            && Set.intersect rockFillDown filled = Set.empty
        then
            therock <- { therock with pos = downPos }
        else
            anotherMoveAllowed <- false

    //printfn "  Elapsed Time 3: %i" timer.ElapsedMilliseconds

    //currentJetStream <- Seq.tail currentJetStream
    //printfn "  Elapsed Time 4: %i" timer.ElapsedMilliseconds

    //printfn "Elapsed Time: %i" timer.ElapsedMilliseconds

    //drawStack { state with rocks = safePrepend therock state.rocks }
    let newfilled = filled + Rock.realcoord therock

    jet, currentIdx, newfilled

let solve rocksnumber (lines: string []) =
    let jet = lines[0]

    //let jetStream = Seq.initInfinite (fun i -> jet[i % jet.Length])

    let rocksStream =
        Seq.initInfinite (fun i ->
            { pattern = RockTypes[i % RockTypes.Length]
              pos = { X = 0; Y = 0 }
              idx = i })
        |> Seq.take rocksnumber

    let (_, _, filled) =
        rocksStream
        |> Seq.take rocksnumber
        |> Seq.fold processRock (jet, -1, Floor)

    let maxY = getHeight filled
    print (rocksnumber, maxY)
    maxY

let solve1 = solve 2022
let solve2 = solve1
