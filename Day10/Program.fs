open System.IO

let input = File.ReadAllLines(@"../../../input.txt") |> Array.map int |> Array.sort

let updateCount (count: Map<int, int>) diff =
    if count.ContainsKey(diff) then
        count.Add(diff, count.[diff] + 1)
    else
        count.Add(diff, 1)

let rec findChain jolt (count: Map<int, int>) adapters =
    let compatibleAdapters =
        adapters
        |> Array.mapi (fun i x -> (i, x, x - jolt))
        |> Array.filter (fun (_, _, diff) -> diff <= 3)

    if Array.isEmpty compatibleAdapters then
       let finalCount = updateCount count 3
       finalCount.[1] * finalCount.[3]
    else
        let (index, adapter, diff) = Array.head compatibleAdapters
        findChain adapter (updateCount count diff) (adapters.[index+1..])

let part1 = findChain 0 Map.empty input

let part2 = 0

[<EntryPoint>]
let main _ =
    printfn $"Answer part 1: {part1}"
    printfn $"Answer part 2: {part2}"
    0
