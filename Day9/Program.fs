open System.IO

let input = File.ReadAllLines(@"../../../input.txt") |> Array.map int64

let rec XMAS start step (data: int64 array) =
    let numbers = data.[start..start+step-1]
    let candidate = data.[start + step]

    let isAnswer =
        numbers
        |> Array.mapi (fun i x -> i, candidate - x)
        |> Array.filter (fun (i, x) -> Array.contains x numbers)
        |> Array.isEmpty

    if isAnswer then candidate else XMAS (start+1) step data

let part1 = XMAS 25 25 input

let part2 = 
    let intervals = seq { for i in 0..input.Length-1 do
                            for j in i..input.Length-1-i -> input.[i..j] }
    intervals
    |> Seq.map (fun xs -> Array.min xs + Array.max xs, Array.sum xs)
    |> Seq.pick (fun (answer, sum) -> if sum = part1 then Some(answer) else None)

[<EntryPoint>]
let main _ =
    printfn $"Answer part 1: {part1}"
    printfn $"Answer part 2: {part2}"
    0
