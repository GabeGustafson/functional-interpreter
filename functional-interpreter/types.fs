namespace functional_interpreter

// this module defines the types that the interpreter must handle
module types =
    // FUNDAMENTAL TYPES

    type Operator = PLUS | MINUS | TIMES | DIV

    type Name(n: string) = 
        member this.name_data = n

    type Value =
        | IntValue of int32
        | BoolValue of bool
        | VoidValue
        with override this.ToString() =
                match this with
                | IntValue(i) -> i.ToString()
                | BoolValue(b) -> b.ToString()
                | VoidValue -> "VOID"


    // STATEMENTS

    // Expressions evaluate to a Value
    type Expression =
        | IntConstant of int32
        | BinOp of Operator * Expression * Expression
        | Let of Name * Expression * Statement
        | Variable of Name
        | Eq of Expression * Expression
        | Neq of Expression * Expression
        | If of Expression * Statement * Statement
        | FunctionDeclaration of Name * Name[] * Expression * Expression
        | FunctionCall of Name * Expression[]
    and Statement = // Statements do not not evaluate to a Value
        | Expr of Expression
        | Seq of Statement[]
        | SetVar of Name * Expression


    // FUNCTIONS

    type Function(b:Expression, fA:Name[]) =
            member this.body = b
            member this.formalArgs = fA