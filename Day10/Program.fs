open System.IO

let input = File.ReadAllLines(@"../../../input.txt")
            |> Array.map int |> Array.sort |> Array.toList

let getCompatibleAdapters jolt adapters =
    adapters
    |> List.mapi (fun i x -> (i, x, x - jolt))
    |> List.filter (fun (_, _, diff) -> diff <= 3)

let updateCount (count: Map<int, int>) diff =
    match count.TryFind(diff) with
    | Some(value) -> count.Add(diff, value + 1)
    | None -> count.Add(diff, 1)

let rec findChain jolt (count: Map<int, int>) adapters =
    match getCompatibleAdapters jolt adapters with
    | [] -> 
        let finalCount = updateCount count 3
        finalCount.[1] * finalCount.[3]
    | compatibleAdapters -> 
        let (i, adapterJolt, diff) = List.head compatibleAdapters
        findChain adapterJolt (updateCount count diff) adapters.[i+1..]

let rec sumChains jolt adapters =
    match getCompatibleAdapters jolt adapters with
    | [] -> 1L
    | [(i, adapterJolt, _)] -> sumChains adapterJolt adapters.[i+1 ..]
    | [(i, adapterJolt, _); _] -> 2L * sumChains adapterJolt adapters.[i+1..]
    | [_; (i, iAdapterJolt, _); (j, jAdapterJolt, _)] ->
        3L * sumChains iAdapterJolt adapters.[i+1..] + sumChains jAdapterJolt adapters.[j+1..]

let part1 = findChain 0 Map.empty input
let part2 = sumChains 0 input

[<EntryPoint>]
let main _ =
    printfn $"Answer part 1: {part1}"
    printfn $"Answer part 2: {part2}"
    0
