open System
open System.IO
open System.Text.RegularExpressions

let input =
    let separator = $"{Environment.NewLine}{Environment.NewLine}"
    File.ReadAllText(@"../../../input.txt").Split(separator)

let createPassport (collection: GroupCollection) =
    let keys = collection.[2].Captures;
    let values = collection.[3].Captures

    [for i in 0..keys.Count-1 -> keys.[i].Value, values.[i].Value]
    |> Map.ofList

let passports =
    Array.map (fun str -> Regex.Match(str, @"((\w{3}):(\S+)\s*)+")) input
    |> Array.map (fun x -> createPassport x.Groups)

let passportsWithNecessaryFields =
    let isValid (passport: Map<string, string>) = 
        passport.Count = 8
        || (passport.Count = 7 && not (passport.ContainsKey "cid"))

    Array.filter isValid passports

let validateYear min max (value: string) =
    Regex.Match(value, "^\d{4}$").Success
    && int value >= min && int value <= max

let heightLimits =
    Map.empty
        .Add("cm", (150, 193))
        .Add("in", (59, 76))

let validateHeight (value: string) =
    let matching = Regex.Match(value, "^(\d+)(cm|in)$")

    if matching.Success then
        let height = int matching.Groups.[1].Value
        let unit = matching.Groups.[2].Value
        let min, max = heightLimits.[unit]
        height >= min && height <= max
    else false

let eyeColors = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]

let validateEyeColor (value: string) =
    List.contains value eyeColors

let validatePassportID (value: string) =
    Regex.Match(value, @"^\d{9}$").Success

let validateHairColor (value: string) =
    Regex.Match(value, "(^#[a-f0-9]{6}$)").Success

let validationFunctions =
    Map.empty
        .Add("byr", validateYear 1920 2002)
        .Add("iyr", validateYear 2010 2020)
        .Add("eyr", validateYear 2020 2030)
        .Add("hgt", validateHeight)
        .Add("hcl", validateHairColor)
        .Add("ecl", validateEyeColor)
        .Add("pid", validatePassportID)
        .Add("cid", fun _ -> true)

let validatePassportFields (passport: Map<string, string>) =
    Map.fold (fun res key value -> res && (validationFunctions.[key] value)) true passport

let validPassports =
    passportsWithNecessaryFields
    |> Array.filter validatePassportFields

let part1 =
    passportsWithNecessaryFields |> Array.length

let part2 =
    validPassports |> Array.length 

[<EntryPoint>]
let main _ =
    printfn $"Answer part 1: {part1}"
    printfn $"Answer part 2: {part2}"
    0
