open System
open System.IO
open System.Text.RegularExpressions

let input =
    let separator = $"{Environment.NewLine}{Environment.NewLine}"
    File.ReadAllText(@"../../../input.txt").Split(separator)

let createPassportMap (collection: MatchCollection) =
    [for x in collection -> x.Groups.[1].Value, x.Groups.[2].Value]
    |> Map.ofList

let passports =
    input
    |> Array.map (fun str -> Regex.Matches(str, @"(?:(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):(\S+))"))
    |> Array.map (fun fields -> createPassportMap fields)

let passportsWithNecessaryFields =
    let isNorthPoleCredentials (passport: Map<string, string>) =
        passport.Count = 7 && not (passport.ContainsKey "cid")

    let hasNecessaryFields (passport: Map<string, string>) = 
        passport.Count = 8 || isNorthPoleCredentials passport

    Array.filter hasNecessaryFields passports

let validateYear min max (value: string) =
    let inRange year =
        year >= min && year <= max

    Regex.Match(value, "^\d{4}$").Success && inRange (int value)

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
    Regex.Match(value, "^#[a-f0-9]{6}$").Success

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

let hasValidFields =
    Map.forall (fun key value -> validationFunctions.[key] value)

let validPassports =
    passportsWithNecessaryFields |> Array.filter hasValidFields

[<EntryPoint>]
let main _ =
    printfn $"Answer part 1: {passportsWithNecessaryFields.Length}"
    printfn $"Answer part 2: {validPassports.Length}"
    0
