open System.IO
open System.Text.RegularExpressions

let input =
    let extractValues str = 
        Regex.Match(str, @"(\d+)-(\d+) (\w): (\w+)").Groups
        |> fun groups -> (int groups.[1].Value, int groups.[2].Value, char groups.[3].Value, groups.[4].Value)

    File.ReadAllLines("../../../input.txt")
    |> Array.map extractValues

let part1 =
    let count c = Seq.filter ((=) c) >> Seq.length

    input
    |> Array.map (fun (min, max, c, password) -> (min, max, (count c password)))
    |> Array.filter (fun (min, max, count) -> count >= min && count <= max)
    |> Array.length

let part2 =
    let isValid (index1: int, index2: int, c: char, password: string) =
        let matchingIndices = [for i in 1..password.Length do if password.[i - 1] = c then i]
        List.exists ((=) index1) matchingIndices <> List.exists ((=) index2) matchingIndices

    input
    |> Array.filter isValid
    |> Array.length

[<EntryPoint>]
let main _ =
    printfn $"Answer part 1: {part1}"
    printfn $"Answer part 2: {part2}"
    0