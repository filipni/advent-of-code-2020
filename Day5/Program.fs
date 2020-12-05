open System.IO
open System.Text.RegularExpressions

let input =
    let parseInput (str: string) =
        let matching = Regex.Match(str, @"([FB]{7})([RL]{3})")
        matching.Groups.[1].Value |> Seq.toList,
        matching.Groups.[2].Value |> Seq.toList

    File.ReadAllLines(@"../../../input.txt") |> Array.map parseInput |> Array.toList

let isUpperHalf c = 
    c = 'B' || c = 'R'

let rec getPosition parts positions =
    match parts, positions with
    | _, [r] -> r
    | x::xs, _ ->
        let mid = positions.Length / 2
        let positionsToKeep = if (isUpperHalf x) then positions.[mid..] else positions.[..mid-1]
        getPosition xs positionsToKeep

let rows = [0..127]
let columns = [0..7]

let getSeatId rowInstructions columnInstructions =
    let row = getPosition rowInstructions rows
    let column = getPosition columnInstructions columns
    row * 8 + column

let seatIds =
    List.map (fun (x, y) -> getSeatId x y) input

let minSeatId = List.min seatIds
let maxSeatId = List.max seatIds 

let missingSeatIds =
    [minSeatId..maxSeatId]
    |> List.except seatIds

[<EntryPoint>]
let main _ =
    printfn $"Answer part 1: {maxSeatId}"
    printfn $"Answer part 2: {missingSeatIds.[0]}"
    0
