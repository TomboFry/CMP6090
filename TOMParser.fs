/// TOM - Short for TOM's Opinionated Mess
module TOMParser

open ParserLibrary
open AbstractSyntaxTree

(*
    FORWARD REFERENCES:
    Create forward references to everything we might need code for
*)

let parseExpression, parseExpressionRef = createParserReference<Expression>
let parseValue, parseValueRef = createParserReference<Atomic>
let parseStatement, parseStatementReference = createParserReference<Statement>

(*
    INITIAL CHARACTER SET UP:
    Includes digits, the alphabet (upper/lower), whitespace, and
    various symbols like brackets 
*)

let digit = parseCharList [ '0' .. '9' ]
            |>? "digit"

let alphaLower = parseCharList [ 'a' .. 'z' ]
let alphaUpper = parseCharList [ 'A' .. 'Z' ]

let alpha = alphaLower <|> alphaUpper

let whitespace = parseCharList [ '\n'; '\t'; '\r'; ' '; ] |>? " "
let manyWs = parseMany whitespace
let alphanumeric = digit <|> alpha
                   |>? "alphanumeric"

/// "
let quote = parseChar '\"'
/// '
let apostrophe = parseChar '\''
/// $
let dollar = parseChar '$'
/// /
let slashFd = parseChar '/'
/// \
let slashBk = parseChar '\\'

/// [
let bkSqLeft = parseChar '['
/// ]
let bkSqRight = parseChar ']'

/// (
let bkLeft = parseChar '('
/// )
let bkRight = parseChar ')'

/// {
let bkClLeft = parseChar '{'
/// }
let bkClRight = parseChar '}'

/// <
let bkAgLeft = parseChar '<'
/// >
let bkAgRight = parseChar '>'

/// ;
let semiColon = parseChar ';'
/// ;
let colon = parseChar ':'
/// ,
let comma = parseChar ','
/// =
let equals = parseChar '='
/// !
let exclamation = parseChar '!'
/// ?
let questionMark = parseChar '?'
/// +
let plus = parseChar '+'
/// +
let minus = parseChar '-'
/// *
let asterisk = parseChar '*'
/// _
let underscore = parseChar '_'

let stringChars =
    parseParserList [
        apostrophe; dollar; semiColon; comma; equals; bkSqLeft; bkSqRight;
        bkAgLeft; bkAgRight; bkClLeft; bkClRight; bkLeft; bkRight; plus;
        exclamation; alphanumeric; whitespace; slashFd; slashBk; asterisk;
        colon; minus; underscore; questionMark;
    ]

let innerString = parseMany stringChars
                  |>> listToString

let parseComment =
    parseString "(*\"" ++ innerString ++ parseString "\"*)"
    |>> ignore

let ws = manyWs ++ parseMany parseComment ++ manyWs
         |>> ignore
         |>? ""

let pWsL p = ws /> p
let pWsR p = p </ ws
let pWsB p = ws /> p </ ws 

(*
    CUSTOM VALUE TYPE SETUP:
    Parses string, numbers, booleans, array, and null
*)

let pBool =
    let parseTrue = parseString "true" |>>% Bool true
    let parseFalse = parseString "false" |>>% Bool false
    parseTrue <|> parseFalse
    |>? "boolean"

let pNumber =
    let parseSign =
        parseOptional (parseString "-" <|> parseString "+")
        |> optToStr
    let digits = parseOneOrMore digit
                 |>> listToString
    let point = parseString "."
    let pointOnwards =
        parseOptional (point ++ digits)
        |>> (fun a ->
            match a with
            | None -> ""
            | Some (b,c) -> b + c
        )

    ((parseSign ++ digits) |>> tupleToString) ++ pointOnwards
    |>> tupleToString
    |>> Number
    |>? "number"

let pString =
    quote /> innerString </ quote
    |>> String
    |>? "string"

let pNull =
    parseString "null"
    |>>% Null

let pArray =
    (bkSqLeft ++ ws) /> parseSeqMany parseValue (pWsB comma) </ pWsB bkSqRight
    |>> Array
    |>? "array"

parseValueRef :=
    parseParserList [
        pNull; pArray; pBool; pNumber; pString;
    ]
    |>? "value"

(*
    VARIABLE DECLARATION:
    Have the ability to declare variables
*)

/// `Name` in `AbstractSyntaxTree.fs`
/// Must begin with a character from the alphabet, then any alphanumeric character is fine
let parseName = alpha ++ parseMany alphanumeric
                |>> (fun (a, b) -> string a + listToString b)
                |>? "name"

/// `VariableName` - eg. $MyVariable
let parseVarName = dollar /> parseName
                   |>> VariableName
                   |>? "$varName"

let parseVarSet =
    (parseVarName </ pWsB equals) ++ parseExpression
    |>> Variable
    |>? " = value"

let parseVarDeclare = parseString "var " /> parseVarSet

(*
    EXPRESSIONS
*)

let parseOpCmp =
    let pEquals =           equals ++ equals
                            |>>% Equal
                            |>? "=="

    let pLessThan =         bkAgLeft
                            |>>% LessThan
                            |>? "<"

    let pLessThanEqual =    bkAgLeft ++ equals
                            |>>% LessThanEqual
                            |>? "<="

    let pGreaterThan =      bkAgRight
                            |>>% GreaterThan
                            |>? ">"

    let pGreaterThanEqual = bkAgRight ++ equals
                            |>>% GreaterThanEqual
                            |>? ">="

    let pNotEqual =         exclamation ++ equals
                            |>>% NotEqual
                            |>? "!="

    pWsB (parseParserList [
              pNotEqual; pEquals; pLessThanEqual; pLessThan;
              pGreaterThanEqual; pGreaterThan
          ])
    |>> Comparison
    |>? "comparison operator"

let parseOpLog =
    let pAnd = parseString "&&"
                |>>% And
    
    let pOr =  parseString "||"
                |>>% Or

    pWsB (pAnd <|> pOr)
    |>> Logical
    |>? "logical operator"

let parseOpMaths =

    let pPlus = parseString "+ "
                |>>% Plus
                |>? " + "

    let pMinus = parseString "- "
                 |>>% Minus
                 |>? " - "

    let pMultiply = parseString "* "
                    |>>% Multiply
                    |>? " * "

    let pDivide = parseString "/ "
                  |>>% Divide
                  |>? " / "

    parseParserList [
        pPlus; pMinus; pMultiply; pDivide
    ]
    |>> Maths
    |>? "mathematical operator"

let parseOperator =
    parseParserList [
        parseOpMaths; parseOpCmp; parseOpLog
    ]

let parseAtomic = parseValue
                  |>> Atomic
                  |>? "value literal"

let parseExpressionVariable = parseVarName
                              |>> EVariable


let parseFnCall =
    let parseParameterList =
        pWsL (parseSeqMany parseExpression (comma ++ ws))
        |>? "(parameter[,parameters])"

    (parseName </ (ws ++ bkLeft)) ++ parseParameterList </ bkRight

let parseExpressionFnCall = parseFnCall |>> EFnCall
     
let parseOperation =
    bkLeft /> parseExpression ++ parseOperator ++ parseExpression </ bkRight
    |>> reduceTupleLeft
    |>> Operation
    |>? "expression"

parseExpressionRef := 
    pWsR (parseParserList [
              parseAtomic;
              parseOperation;
              parseExpressionVariable;
              parseExpressionFnCall;
    ])

(*
    STATEMENTS:
    If statements and variable declaration
*)

/// Even when parsing a single state
let parseStatementSingle = parseStatement
                           |>> (fun x -> [x])
                           |>? " statement"

let parseStatementMultiple =
    (pWsL bkClLeft) /> parseMany parseStatement </ (pWsL bkClRight)
    |>? " { statement(s) } "

let parseStatementBlock =
    parseParserList [
        parseStatementSingle;
        parseStatementMultiple;
    ]

let parseStatementDefinition =
    ws /> parseVarDeclare </ semiColon ++ ws
    |>> Definition

let parseStatementVariableSet =
    ws /> parseVarSet </ semiColon ++ ws
    |>> VariableSet

let parseStatementIf =
    (ws ++ parseString "if ") /> pWsB parseExpression ++ parseStatementBlock
    |>> If
    |>? "if statement"

let parseStatementForEach =
    (pWsL (parseString "foreach") ++ pWsL bkLeft) />
    (pWsB parseVarName </ parseString "in") ++
    (pWsL parseExpression </ pWsL bkRight) ++ parseStatementBlock
    |>> reduceTupleLeft
    |>> ForEach
    |>? "foreach loop"

let parseStatementWhile =
    pWsL (parseString "while") /> pWsB parseExpression ++ parseStatementBlock
    |>> While
    |>? "while loop"

let parseStatementFnCall =
    ws /> parseFnCall </ semiColon
    |>> SFnCall

let parseStatementReturn =
    (ws ++ parseString "return") /> parseOptional (ws /> parseExpression) </ semiColon
    |>> Return

parseStatementReference :=
    parseParserList [
        parseStatementDefinition;
        parseStatementIf;
        parseStatementForEach;
        parseStatementWhile;
        parseStatementReturn;
        parseStatementFnCall;
        parseStatementVariableSet;
    ]
    |>? " statement(s)"

//
//   FUNCTIONS:
//   eg. fn fnName (argA, argB) { (*" some code "*) }
//

// let parseFunction, parseFunctionReference = createParserReference<Function>

let parseParameters = (ws ++ bkLeft) /> parseSeqMany parseVarName (comma ++ ws) </ (bkRight ++ ws)

let parseFunction =
    (ws ++ parseString "fn ") /> parseName ++ parseParameters ++ parseStatementBlock
    |>> reduceTupleLeft
    |>> Function
    |>? "function"

let listLastElement ls =
    List.reduce (fun _ elem -> elem) ls

/// Run the TOM Parser with specified input
let runParser input =
    // let result = parse (parseOneOrMore parseFunction) input
    parse (parseOneOrMore parseFunction) input

    // // Make sure the `main` function appears last in the file, otherwise
    // // throw an error
    // match result with
    // | Err _ -> result
    // | Ok { Value = fnList; Input = _; } ->
    //     let lastFn = listLastElement fnList
    //     let (Function (name, _, _)) = lastFn
    //     if name = "main" then
    //         result
    //     else
    //         Err ("main function", "Missing 'main' function (must be last)")

