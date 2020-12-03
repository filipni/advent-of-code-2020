open System.IO

let input =
    File.ReadAllLines(@"../../../input.txt") |> Array.toList

let rec traversePath pos map treeSum =
    match map with
    | [] -> treeSum
    | (x::xs) ->
        let newPosition = (pos + 3) % String.length x
        let updatedTreeSum = if x.[pos] = '#' then treeSum + 1 else treeSum
        traversePath newPosition xs updatedTreeSum

let part1 =
    traversePath 0 input 0

[<EntryPoint>]
let main _ =
    printfn "Answer part 1: %d" part1 
    0
