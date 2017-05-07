module AbstractSyntaxTree

type Name = string

type Value =
    | VString of string
    | VBool of bool
    | VNumber of float
    | VArray of Value list
    | VNull

let VNumber str =
    match System.Double.TryParse(str) with
    | (true, float) -> VNumber float
    | _ -> VNumber 0.0

type VariableName = VariableName of Name

type OperatorLogical =
    | And
    | Or

type OperatorComparison =
    | Equal
    | LessThan
    | LessThanEqual
    | GreaterThan
    | GreaterThanEqual
    | NotEqual

type OperatorMaths =
    | Plus
    | Minus
    | Multiply
    | Divide

type Operator =
    | Logical of OperatorLogical       // &&, ||
    | Comparison of OperatorComparison // ==, <, <=, >, >=, !=
    | Maths of OperatorMaths           // +, -, *, /

type Expression =
    | Atomic of Value
    | EVariable of VariableName
    | Operation of Expression * Operator * Expression
    | EFnCall of FunctionCall

and FunctionCall = Name * Expression list

type Variable = Variable of VariableName * Expression

type Statement =
    | Definition of Variable
    | If of Expression * StatementBlock
    | ForEach of VariableName * Expression * StatementBlock
    | While of Expression * StatementBlock
    | SFnCall of FunctionCall
    | Return of Expression option

and StatementBlock = Statement list

type Parameter = VariableName

type Function = Function of Name * Parameter list * StatementBlock
