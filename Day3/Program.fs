open System.IO

let input =
    File.ReadAllLines(@"../../../input.txt") |> Array.toList

let rec updateMap yDiff map = 
    match yDiff, map with
    | 0, _ -> map
    | _, [] -> []
    | _, (_::xs) -> updateMap (yDiff - 1) xs 

let rec traversePath pos map xDiff yDiff treeSum =
    match map with
    | [] -> treeSum
    | (x::_) ->
        let newPosition = (pos + xDiff) % String.length x
        let updatedTreeSum = if x.[pos] = '#' then treeSum + 1 else treeSum
        let updatedMap = updateMap yDiff map
        traversePath newPosition updatedMap xDiff yDiff updatedTreeSum

let part1 =
    traversePath 0 input 3 1 0

let part2 =
    [
        traversePath 0 input 1 1 0
        traversePath 0 input 3 1 0
        traversePath 0 input 5 1 0
        traversePath 0 input 7 1 0
        traversePath 0 input 1 2 0
    ]
    |> List.map int64
    |> List.reduce (fun x y -> x * y) 

[<EntryPoint>]
let main _ =
    printfn "Answer part 1: %d" part1 
    printfn "Answer part 2: %d" part2 
    0
