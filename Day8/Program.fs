open System.IO
open System.Text.RegularExpressions

let defaultProgram =
    let parseInput (matching: Match) =
        matching.Groups.[1].Value, int matching.Groups.[2].Value

    let pattern = @"(nop|acc|jmp) \+?(-?\d+)"
    let extractOperation = (fun str -> Regex.Match(str, pattern)) >> parseInput

    let input = File.ReadAllLines(@"../../../input.txt") |> Array.toList
    
    input 
    |> List.map extractOperation
    |> List.zip [0..input.Length-1]
    |> Map.ofList

type State = { pc: int; acc: int }
let defaultState = { pc = 0; acc = 0 } 

let updateState state op = 
    match op with
    | "acc", arg -> { acc = state.acc + arg; pc = state.pc + 1 }
    | "jmp", arg -> { state with pc = state.pc + arg } 
    | "nop", _ -> { state with pc = state.pc + 1 }
    
let rec runProgram (state: State) (history: int Set) (program: Map<int, string * int>) =
    if history.Contains(state.pc) || state.pc = defaultProgram.Count then
        state
    else
        let op = program.[state.pc]
        runProgram (updateState state op) (history.Add(state.pc)) program

let switchOp op =
    match op with
    | "jmp", arg -> ("nop", arg)
    | "nop", arg -> ("jmp", arg)
    | _ -> op

let rec allPossiblePrograms pcs (programList: Map<int, string * int> List) =
    match pcs with
    | [] -> programList
    | x::xs -> 
        let oldOp = defaultProgram.[x]
        let newOp = switchOp oldOp
        if newOp <> oldOp then
            let newProgram = defaultProgram.Add(x, switchOp defaultProgram.[x])
            allPossiblePrograms xs (newProgram::programList)
        else
            allPossiblePrograms xs programList

let part1 =
    let endState = runProgram defaultState Set.empty defaultProgram
    endState.acc

let part2 =
    let endStates =
        allPossiblePrograms [0..defaultProgram.Count-1] []
        |> List.map (runProgram defaultState Set.empty)
        |> List.filter (fun state -> state.pc = defaultProgram.Count)
    endStates.[0].acc

[<EntryPoint>]
let main _ =
    printfn $"Answer part 1: {part1}"
    printfn $"Answer part 2: {part2}"
    0
