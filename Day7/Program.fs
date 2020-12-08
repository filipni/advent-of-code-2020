open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines(@"../../../input.txt")

let createRule (matching: Match) =
    let content (capacityGroup: Group) (bagGroup: Group) = 
        let bags = [for x in bagGroup.Captures -> x.Value]
        let capacities = [for x in capacityGroup.Captures -> int x.Value]
        List.zip bags capacities

    (matching.Groups.[1].Value, content matching.Groups.[2] matching.Groups.[3])

let rules =
    let pattern = @"(\w+ \w+) bags contain(?: (\d+) (\w+ \w+) bags?,?| no other bag)+"
    let extractRule = (fun str -> Regex.Match(str, pattern)) >> createRule

    input |> Array.map extractRule |> Map.ofArray

let rec containsShinyGold (content: (string * int) list) =
    match content with
    | [] -> false
    | (bag, _)::xs -> bag = "shiny gold" || containsShinyGold (xs @ rules.[bag]) 

let repeat n =
    List.replicate n >> List.concat

let rec countBags sum (content: (string * int) list) =
    match content with
    | [] -> sum
    | (bag, capacity)::xs -> countBags (sum + capacity) (xs @ repeat capacity rules.[bag]) 

let part1 =
    rules |> Map.filter (fun _ value -> containsShinyGold value) |> Map.count

let part2 =
    countBags 0 rules.["shiny gold"] 

[<EntryPoint>]
let main _ =
    printfn $"Answer part 1: {part1}"
    printfn $"Answer part 2: {part2}"
    0
