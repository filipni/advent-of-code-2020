open System
open System.IO

let groups =
    let separator = $"{Environment.NewLine}{Environment.NewLine}"
    File.ReadAllText(@"../../../input.txt").Split(separator)

let anyoneYesQuestions (group: string) =
    group.Replace(Environment.NewLine, "") |> Set.ofSeq

let everyoneYesQuestions (group: string) =  
    let count c = Seq.filter ((=) c) >> Seq.length
    let numGroupMembers = group.Split(Environment.NewLine).Length

    anyoneYesQuestions group
    |> Set.filter (fun c -> (count c group) = numGroupMembers)

let foldGroups questionFilter =
    groups
    |> Array.map (questionFilter >> Set.count)
    |> Array.sum 

[<EntryPoint>]
let main _ =
    printfn $"Answer part 1: {foldGroups anyoneYesQuestions}"
    printfn $"Answer part 2: {foldGroups everyoneYesQuestions}"
    0
