module CMP6090.Project
open ParserLibrary
open AbstractSyntaxTree
open TOMParser
open System

let parseFile filePath str =
    printfn "\n\nPARSING FILE: %s\n" filePath

    let rec inner input =
        let result = runParser input
        match result with
        | Err (label, err) ->
            printfn "Could not parse\n%s\nError:\n%s" label err 
        | Ok { Value = value; Input = leftOver; } ->
            printfn "Abstract Syntax Tree:\n%A\n" value
            if not (String.IsNullOrWhiteSpace(leftOver)) then
                printfn "Left Over Code:\n```\n%s\n```" (leftOver.Trim())
            else
                printfn "End of File"

    inner str


[<EntryPoint>]
let main args =

    // Get a file passed into the program and parse it
    args
    |> Array.map (fun a -> (a, System.IO.File.ReadAllText(a)))
    |> Array.map (fun (a, b) -> parseFile a b)
    |> ignore

    0
