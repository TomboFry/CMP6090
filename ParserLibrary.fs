/// Main Parser Library
module ParserLibrary

open System

let rec sleep counter =
    if counter > 0.1 then
        sleep (counter - 0.000005)
    else
        ()

/// ParseValue - The type stored by a successful parse attempt
type ParseValue<'a> =
    { Value: 'a; Input: string; }

    // Map a function to the successly parsed return value, returning a
    // new ParseValue in the process.
    member this.Map func =
        { Value = func this.Value; Input = this.Input; }

    // Similar to above but return the value returned from the function
    // that was applied
    member this.Bind func = func this.Value;

/// Create a new ParseValue, much shorter than the Unit function above.
let ParseValue value input =
    {
        Value = value;
        Input = input;
    }

type ErrLabel = string
type ErrReason = string

/// Result type, allows for checking if a parse attempt was successful or not
type Result<'a> =
    | Ok of ParseValue<'a>
    | Err of ErrLabel * ErrReason

    member this.Map func =
        match this with
        | Ok { Value = value; Input = input; } ->
            Ok (ParseValue (func value) input)
        | Err (label, err) -> Err (label, err)

    member this.isErr =
        // "Consider changing `isErr` to Pascal case"
        // How about shut up, and also not give Title Case a stupid name.
        match this with
        | Err _ -> true
        | _ -> false

    member this.isOk =
        match this with
        | Ok _ -> true
        | _ -> false

/// Parser type, contains the function to run to parse, and a label defining
/// what it is we're actually parsing
type Parser<'a> =
    {
        func: (string -> Result<'a>);
        label: string;
    }

    member this.Return =
        this.func

    member this.Map f =
        let { func = func; label = label; } = this
        let newFunc = (fun input ->
            (func input).Map f
        )
        { func = newFunc; label = label; }

/// Creates a new Parser with its values
let Parser func label = {
    func = func; label = label;
}

/// Convert a list of parser results (chars or strings) into a single string
let listToString (list:List<'a>) =
    List.fold (fun acc elem ->
        let elemStr =
            match box elem with
            | :? String as s -> s
            | :? Char as c -> string c
            | _ -> ""
        acc + elemStr
    ) "" list

let tupleToString tuple =
    let (a, b) = tuple
    match (box a, box b) with
    | (:? string as sa), (:? string as sb) -> sa + sb
    | (:? string as sa), (:? char as cb) -> sa + string cb
    | (:? char as ca), (:? string as sb) -> string ca + sb
    | (:? char as ca), (:? char as cb) -> string ca + string cb
    | _ -> ""

let reduceTupleLeft (tuple:('a * 'b) * 'c) =
    let ((a, b), c) = tuple
    a, b, c

let reduceTupleRight (tuple:'a * ('b * 'c)) =
    let (a, (b, c)) = tuple
    a, b, c

/// Attempt to parse the parser and return the result it got from doing so.
let parse (parser:Parser<'a>) input =
    parser.func input

/// Parse a single character
let parseChar character =
    let label = string character
    let parser input =
        if String.IsNullOrEmpty(input) then
            Err (label, "End of input")
        else
            let firstCharacter = input.[0]
            if firstCharacter = character then
                Ok (ParseValue character input.[1..])
            else
                Err (label, (sprintf "Unexpected '%c'" firstCharacter))
    Parser parser label

/// Attempt to parse parser A. If that fails, attempt parser B
let parseOr pA pB =
    let label = sprintf "%s|%s" pA.label pB.label
    let parser input =
        let resultA = parse pA input
        match resultA with
        | Ok _ ->  resultA
        | Err _ ->
            let resultB = parse pB input
            match resultB with
            | Ok _ -> resultB
            | Err (_, err) ->
                Err (label, err)
    Parser parser label

/// Require that both parsers succeed
let parseAnd pA pB =
    let label = sprintf "%s%s" pA.label pB.label
    let parser input =
        match (parse pA input) with
        | Err (_, err) -> Err (label, err)
        | Ok {Value = valueA; Input = inputA} ->
            match (parse pB inputA) with
            | Err (_, err) -> Err (label, err)
            | Ok { Value = valueB; Input = inputB} ->
                Ok (ParseValue (valueA, valueB) inputB)
    Parser parser label

/// Takes a function and a parser and applies the function to the value
/// returned by the parser (assuming there were no errors)
let parserApply func pA =
    let parser input =
        let result = parse pA input
        match result with
        | Err (label, err) -> Err (label, err)
        | Ok { Value = value; Input = remaining } ->
            let funcResult = func value
            Ok (ParseValue funcResult remaining)
    Parser parser pA.label

/// Loop through a list of parsers, and return a list of the results (and the
/// remaining input to parse
let rec loop pA input =
    let emptyReturn = ([], input)
    let result = parse pA input
    match result with
    | Err _ -> emptyReturn
    | Ok { Value = newValue; Input = newInput } ->
        let (finalValue, finalInput) = loop pA newInput
        (newValue::finalValue, finalInput)


/// Parsing zero or more doesn't matter if it can find any characters or not
/// and so will return Ok regardless of the output
let parseMany pA =
    let parser input = // Ok (ParseValue.Unit (loop pA input))
        let (resList, resInput) = loop pA input
        Ok (ParseValue resList resInput)
    Parser parser pA.label

/// Parsing one or more requires only the first to succeed. Once the first
/// succeeds then we don't care how many it parses afterwards, like `parseMany`
let parseOneOrMore pA =
    let parser input =
        let result = parse pA input
        match result with
        | Err (label, err) -> Err (label, err)
        | Ok { Value = newValue; Input = newInput } ->
            let (finalValue, finalInput) = loop pA newInput
            let outputValue = newValue::finalValue
            Ok (ParseValue outputValue finalInput)
    Parser parser pA.label

let parseOptional pA =
    let parser input =
        let result = parse pA input
        match result with
        | Ok { Value = value; Input = newInput; } ->
            Ok (ParseValue (Some value) newInput)
        | Err _ ->
            Ok (ParseValue None input)
    
    Parser parser pA.label

/// Infix notation for setting the label on a parser.
/// Returns a new Parser with the same function but different label
let ( |>? ) parser newLabel =
    let newParser input =
        let result = parse parser input
        match result with
        | Ok _ -> result
        | Err (_, err) ->
            Err (newLabel, err)
    { func = newParser; label = newLabel; }

/// Run two parsers that both need to succeed,
/// but discard whatever's on the left
let parseKeepLeft pA pB =
    parseAnd pA pB
    |> parserApply (fun (a, _) -> a)
    |>? pA.label

/// Run two parsers that both need to succeed,
/// but discard whatever's on the right.
let parseKeepRight pA pB =
    parseAnd pA pB
    |> parserApply (fun (_, b) -> b)
    |>? pB.label


/// Takes in three parsers and only returns the result of the middle one
/// (Useful for quotes, brackets, and whitespace)
let parseBetween pA pB pC =
    parseKeepLeft (parseKeepRight pA pB) pC
    |>? pB.label

/// Convert a list of parsers into a single parser, making sure any one of
/// them succeed.
let parseParserList parserList =
    List.reduce parseOr parserList

let parserCreate value =
    let label = sprintf "%A" value
    let parser input = Ok (ParseValue value input)
    Parser parser label

/// Matches on an entire string
let parseString value =
    let parser input =
        if String.IsNullOrEmpty input then
            Err (value, (sprintf "Unexpected `%s`, expecting `%s`" input value))
        else
            let newParser = value
                            |> List.ofSeq
                            |> List.map parseChar
                            |> List.fold (fun acc elem ->
                                parseAnd acc elem
                                |> parserApply (fun (a, b) -> a + string b)
                            ) (parserCreate "")
            parse newParser input
    Parser parser value

/// Turn a list of characters into a list of character parsers
let parseCharList (charList:char list) =
    charList
    |> List.map parseChar
    |> parseParserList

/// Turn a list of strings into a list of string parsers
let parseStringList (stringList:string list) =
    stringList
    |> List.map parseString
    |> parseParserList


/// Create infix notation for various methods here to make life easier
let ( ++ ) = parseAnd               // ++ makes sure both sides parse

let ( <|> ) = parseOr               // <|> makes sure either side parses

let ( /> ) = parseKeepRight         // >> Parse both but only keep right side

let ( </ ) = parseKeepLeft          // << Parse both but only keep left side

let ( |>> )(parser:Parser<'a>) func = // Apply a function to the result returned
    parser.Map func                   // by a parser function

let ( |>>% ) parser output =         // When we don't care about the output of 
    parser |>> (fun _ -> output)     // the parser

/// Parse something **at least once** followed by a separator parser, excluding
/// the last element.
/// eg. parser = "ab", separator = ','
/// Returns Successfully: `ab,ab,ab`
let parseSeq parser separator =
    parser ++ parseMany (separator /> parser)
    |>> fun (p, pList) -> p::pList
    |>? parser.label + " [" + separator.label + parser.label + "]"

/// Parse something **zero or more** times followed by a separator parser,
/// excluding the last element
/// eg. parser = "ab", separator = ','
/// Returns Successfully: `ab,ab,ab`
let parseSeqMany parser separator =
    let pSeq = parseSeq parser separator
    parseParserList [
        pSeq;
        parserCreate [];
    ]
    |>? pSeq.label


let optToStr (pA:Parser<string option>) =
    pA
    |>> (fun res ->
        match res with
        | None -> ""
        | Some str -> str
    )

/// Create a reference to a parser we have not yet created, so we can have
/// parsers inside themselves. Essentially forward referencing.
let createParserReference<'a> =

    let label = "undefined"

    let parserReferenceFunction = 
        let parser _:Result<'a> = Err (label, "Cannot parse undefined parser")
        Parser parser label
    
    // Create a reference to the parser placeholder function
    let parserReference = ref parserReferenceFunction

    // Create the inner parser function like usual...
    let parser input = 
        // ... and pass all the input into the parser reference
        parse !parserReference input 

    let wrapperParser = Parser parser label

    wrapperParser, parserReference

