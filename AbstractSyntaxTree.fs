module AbstractSyntaxTree

type Name = string

type Atomic =
    | String of string
    | Bool of bool
    | Number of float
    | Array of Atomic list
    | Null

let Number str =
    match System.Double.TryParse(str) with
    | (true, float) -> Number float
    | _ -> Number 0.0

type VariableName = VariableName of Name

type Logical =
    | And              // &&
    | Or               // ||

type Comparison =
    | Equal            // ==
    | LessThan         // <
    | LessThanEqual    // <=
    | GreaterThan      // >
    | GreaterThanEqual // >=
    | NotEqual         // !=

type Maths =
    | Plus             // +
    | Minus            // -
    | Multiply         // *
    | Divide           // /

type Operator =
    | Logical of Logical
    | Comparison of Comparison
    | Maths of Maths

type Expression =
    | Atomic of Atomic
    | EVariable of VariableName
    | Operation of Expression * Operator * Expression
    | EFnCall of FunctionCall

and FunctionCall = Name * Expression list

type Variable = Variable of VariableName * Expression

type Statement =
    | Definition of Variable
    | VariableSet of Variable
    | If of Expression * StatementBlock
    | ForEach of VariableName * Expression * StatementBlock
    | While of Expression * StatementBlock
    | SFnCall of FunctionCall
    | Return of Expression option

and StatementBlock = Statement list

type Parameter = VariableName

type Function = Function of Name * Parameter list * StatementBlock
