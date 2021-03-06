﻿open System.IO

let input =
    File.ReadAllLines(@"../../../input.txt") |> Array.toList

let rec traversePathRecursive xPos map xDiff yDiff sum =
    match map with
    | [] -> sum
    | x::_ ->
        let newXPos = (xPos + xDiff) % String.length x
        let newSum = if x.[xPos] = '#' then sum + 1L else sum
        traversePathRecursive newXPos map.[yDiff..] xDiff yDiff newSum

let traversePath (map: string list) xDiff yDiff =
    let numPositions = if yDiff = 1 then map.Length else map.Length / yDiff + 1
    let positions = [for i in 0..numPositions-1 -> i * xDiff % map.[0].Length, i * yDiff]
    List.sumBy (fun (x, y) -> if map.[y].[x] = '#' then 1L else 0L) positions

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
    printfn $"Answer part 1: {part1}"
    printfn $"Answer part 2: {part2}" 
    0
