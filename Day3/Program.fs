open System.IO

let input =
    File.ReadAllLines(@"../../../input.txt") |> Array.toList

let rec traversePathRecursive xPos map xDiff yDiff (sum: int64) =
    match map with
    | [] -> sum
    | x::_ ->
        let newXPos = (xPos + xDiff) % String.length x
        let newSum = if x.[xPos] = '#' then sum + 1L else sum
        traversePathRecursive newXPos map.[yDiff..] xDiff yDiff newSum

let traversePath (map: string list) xDiff yDiff =
    let numPositions = if yDiff = 1 then map.Length else map.Length / yDiff + 1
    [for i in 0..numPositions-1 -> (i * xDiff % map.[0].Length, i * yDiff)]
    |> List.fold (fun sum (x, y) -> if map.[y].[x] = '#' then sum + 1L else sum) 0L

let part1 =
    traversePath input 3 1

let part2 =
    [
        traversePath input 1 1
        traversePath input 3 1
        traversePath input 5 1
        traversePath input 7 1
        traversePath input 1 2
    ]
    |> List.reduce (fun x y -> x * y) 

[<EntryPoint>]
let main _ =
    printfn "Answer part 1: %d" part1 
    printfn "Answer part 2: %d" part2 
    0
