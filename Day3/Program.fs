open System.IO

let input =
    File.ReadAllLines(@"../../../input.txt") |> Array.toList

let rec traversePath pos map treesHit =
    match map with
    | [] -> treesHit
    | (x::xs) -> traversePath ((pos + 3) % String.length x) xs (if x.[pos] = '#' then treesHit + 1 else treesHit)

let part1 =
    traversePath 0 input 0

[<EntryPoint>]
let main _ =
    printfn "Answer part 1: %d" part1 
    0
