open System
open System.IO

let width, height =
    let inputRows = File.ReadAllLines(@"../../../input.txt") 
    inputRows.Length, inputRows.[0].Length

let indexToPosition index = index % width, index / width

type state = { empty: Set<int * int>; occupied: Set<int * int>; floor: Set<int * int> }
let defaultState = { empty = Set.empty; occupied = Set.empty; floor = Set.empty }

let parseInput state (index, c) =
    let position = indexToPosition index
    match c with
    | '#' -> { state with occupied = state.occupied.Add(position) }
    | 'L' -> { state with empty = state.empty.Add(position) }
    | _ -> { state with floor = state.floor.Add(position) }

let startingState = 
    File.ReadAllText(@"../../../input.txt").Replace(Environment.NewLine, "")
    |> Seq.indexed |> Seq.fold parseInput defaultState

let adjacentPositions (x, y) =
    [ for i in [x-1..x+1] do
        for j in [y-1..y+1] do
            if (i, j) <> (x, y) then (i, j) ]

let countNeighbours state pos =
    adjacentPositions pos
    |> List.filter (fun x -> state.occupied.Contains(x))
    |> List.length
    
let rec runModel state =
    let emptyToOccupied =
        state.empty |> Set.filter (fun pos -> countNeighbours state pos = 0) 
    let stillEmpty = state.empty - emptyToOccupied

    let occupiedToEmpty =
        state.occupied |> Set.filter (fun pos -> countNeighbours state pos >= 4)
    let stillOccupied = state.occupied - occupiedToEmpty

    let empty = stillEmpty + occupiedToEmpty
    let occupied = stillOccupied + emptyToOccupied

    let nextState = { state with empty = empty; occupied = occupied } 
    if state = nextState then state else runModel nextState 

let part1 =
    let finalState = runModel startingState
    finalState.occupied.Count

[<EntryPoint>]
let main _ =
    printfn $"Answer part 1: {part1}"
    0
