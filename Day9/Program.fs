open System.IO

let input =
    File.ReadAllLines(@"../../../input.txt") |> Array.map int64

let rec XMAS start step (data: int64 array) =
    let numbers = data.[start..start+step-1]
    let candidate = data.[start + step]

    let isAnswer =
        numbers
        |> Array.map ((-) candidate)
        |> Array.indexed
        |> Array.filter (fun (i, x) -> (numbers.[i] <> x) && (Array.contains x numbers))
        |> Array.map (fun (_, x) -> x)
        |> Array.isEmpty

    if isAnswer then candidate else XMAS (start+1) step data

let part1 = XMAS 25 25 input

let part2 =
    let mutable sum = 0L
    for i in 0..input.Length-1 do
        for j in i..input.Length-1-i do
            sum <- sum + input.[j]
            if sum = part1 then
                let min = Array.min input.[i..j]
                let max = Array.max input.[i..j]
                printfn $"Answer part 2: {min + max}"
        sum <- 0L

[<EntryPoint>]
let main _ =
    printfn $"Answer part 1: {part1}"
    0
