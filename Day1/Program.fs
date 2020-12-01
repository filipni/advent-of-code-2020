open System.IO

let input =
    let inputPath = @"..\..\..\input.txt"
    File.ReadAllLines(inputPath) |> Array.map int |> Array.toList

let rec combinations n xs =
    match n, xs with
    | 0, _ ->  [[]]
    | _, [] -> []
    | n, (x::xs) -> (List.map ((@) [x]) (combinations (n-1) xs)) @ (combinations n xs)

let solution n sum =
    combinations n input 
    |> List.pick (fun xs -> if List.sum xs = sum then Some xs else None)
    |> List.reduce (fun x y -> x * y)

[<EntryPoint>]
let main _ =
    printfn "Answer part 1: %d" (solution 2 2020)
    printfn "Answer part 2: %d" (solution 3 2020)
    0
