module aoc.day01.solutions

open aoc.common

let solve lines =
    let convertToInt = List.map (List.map (fun (x: string) -> int x))

    let readCalories =
        splitByCond (fun line -> line = "")
        >> convertToInt

    let calories = readCalories <| lines

    let totals = calories |> List.map List.sum
    let max = List.max (totals) //74198

    let totals_sorted = totals |> List.sortDescending
    let max3 = totals_sorted |> List.take (3) |> List.sum //209914
    (max, max3)
