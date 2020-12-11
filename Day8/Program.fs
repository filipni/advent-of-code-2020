open System.IO
open System.Text.RegularExpressions

let defaultProgram =
    let parseInput str = 
        let matching = Regex.Match(str, @"(nop|acc|jmp) \+?(-?\d+)")
        matching.Groups.[1].Value, int matching.Groups.[2].Value

    File.ReadAllLines(@"../../../input.txt")
    |> Seq.mapi (fun i str -> i, parseInput str)
    |> Map.ofSeq

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
    | "jmp", arg -> "nop", arg
    | "nop", arg -> "jmp", arg
    | _ -> op

let allPossiblePrograms =
    List.replicate defaultProgram.Count defaultProgram
    |> List.mapi (fun i program -> program.Add(i, switchOp program.[i]))
    |> List.distinct

let stateAfterFirstLoop =
    runProgram defaultState Set.empty defaultProgram

let stateAfterProgramTermination =
    allPossiblePrograms
    |> List.map (runProgram defaultState Set.empty)
    |> List.filter (fun state -> state.pc = defaultProgram.Count)
    |> List.head

[<EntryPoint>]
let main _ =
    printfn $"Answer part 1: {stateAfterFirstLoop.acc}"
    printfn $"Answer part 2: {stateAfterProgramTermination.acc}"
    0
