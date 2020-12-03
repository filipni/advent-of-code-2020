open System.IO

let input =
    File.ReadAllLines(@"../../../input.txt") |> Array.toList

let rec traversePath xPos map xDiff yDiff (treesHit: int64) =
    match map with
    | [] -> treesHit
    | (x::_) ->
        let newXPos = (xPos + xDiff) % String.length x
        let updatedTreeSum = if x.[xPos] = '#' then treesHit + 1L else treesHit
        traversePath newXPos map.[yDiff..] xDiff yDiff updatedTreeSum

let part1 =
    traversePath 0 input 3 1 0L

let part2 =
    [
        traversePath 0 input 1 1 0L
        traversePath 0 input 3 1 0L
        traversePath 0 input 5 1 0L
        traversePath 0 input 7 1 0L
        traversePath 0 input 1 2 0L
    ]
    |> List.reduce (fun x y -> x * y) 

[<EntryPoint>]
let main _ =
    printfn "Answer part 1: %d" part1 
    printfn "Answer part 2: %d" part2 
    0
