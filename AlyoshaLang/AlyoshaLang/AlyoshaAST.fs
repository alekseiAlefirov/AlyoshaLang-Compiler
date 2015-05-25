module AlyoshaAST

type program = Program of string * (unionDefinition list) * block

and unionDefinition = string * (string list) * (unionElement list)

and unionElement = string * (string list)

and block = Block of expression list

and statement =
    | LetAssignment of assignment
    | LetRecursiveAssignment of (varId * (varId list) * expression) list
    | Assignment of assignment
    | IfStatement of (expression * block * ((expression * block) list) * (block option))
    | WhileStatement of expression * block
    | WriteStatement of expression
    | MatchStatement of varId * (guard list)

and assignment = 
    | UsualAssignment of varId * expression
    | ReadNum of varId
    | ReadLine of varId
    
and guard = string * (string list) * block

and expression =
    | Statement of statement
    | OrList of (expression list)
    | AndList of (expression list)
    
    | Not of expression

    | IsEqual of expression * expression
    | NotEqual of expression * expression
    
    | Greater of expression * expression
    | Less of expression * expression
    | NotGreater of expression * expression
    | NotLess of expression * expression

    | Sum of (sumSign * expression) list
    | Mult of (mulSign * expression) list
    | Mod of expression * expression
    
    | SequenceExpression of block
    | ExprId of varId
    | Abstraction of (varId list) * expression
    | Application of expression * (expression list)
    | NumVal of System.Int32
    | BoolVal of bool
    | StringVal of string
    | UnitVal

and sumSign =
    | Plus
    | Minus

and mulSign =
    | Mul
    | Div

and varId = string * (int ref) //text and id in Table

let unboxBlock blk =
    match blk with
    | Block x -> x