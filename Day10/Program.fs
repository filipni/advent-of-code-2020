open System.IO

let input = File.ReadAllLines(@"../../../input.txt") |> Array.map int |> Array.sort

let getCompatibleAdapters jolt adapters =
    adapters
    |> Array.mapi (fun i x -> (i, x, x - jolt))
    |> Array.filter (fun (_, _, diff) -> diff <= 3)

let updateCount (count: Map<int, int>) diff =
    match count.TryFind(diff) with
    | Some(value) -> count.Add(diff, count.[diff] + 1)
    | None -> count.Add(diff, 1)

let rec findChain jolt (count: Map<int, int>) adapters =
    match getCompatibleAdapters jolt adapters with
    | [||] -> 
        let finalCount = updateCount count 3
        finalCount.[1] * finalCount.[3]
    | compatibleAdapters -> 
        let (index, adapter, diff) = Array.head compatibleAdapters
        findChain adapter (updateCount count diff) adapters.[index+1..]

let rec sumChains jolt adapters =
    match getCompatibleAdapters jolt adapters with
    | [||] -> 1L
    | compatibleAdapters ->
        let length = Array.length compatibleAdapters
        if length = 2 then
            let (index, adapter, _) = compatibleAdapters.[0]
            2L * sumChains adapter adapters.[index+1..]
        elif length =  3 then
            let (index2, adapter2, _) = compatibleAdapters.[1]
            let (index3, adapter3, _) = compatibleAdapters.[2]
            3L * sumChains adapter2 adapters.[index2+1 ..] + sumChains adapter3 adapters.[index3+1 ..]
        else
            let (index, adapter, _) = compatibleAdapters.[0]
            sumChains adapter adapters.[index+1 ..]

let part1 = findChain 0 Map.empty input
let part2 = sumChains 0 input

[<EntryPoint>]
let main _ =
    printfn $"Answer part 1: {part1}"
    printfn $"Answer part 2: {part2}"
    0
