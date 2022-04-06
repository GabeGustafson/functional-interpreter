namespace functional_interpreter

// this module defines the types that the interpreter must handle
module types =
    // FUNDAMENTAL TYPES

    type Operator = PLUS | MINUS | TIMES | DIV

    type Name(n: string) = 
        member this.name_data = n


    // STATEMENTS

    // Expressions must evaluate to a Value
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

    // Statements may evaluate to Values and may change program state
    and Statement = 
        | Expr of Expression // an Expression is a case of Statement
        | Seq of List<Statement>
        | SetVar of Name * Expression
        | IncVar of Name
        | While of Expression * Statement


    // INTERPRETER TYPES

    type Value =
        | IntValue of int32
        | BoolValue of bool
        | VoidValue
        with override this.ToString() =
                match this with
                | IntValue(i) -> i.ToString()
                | BoolValue(b) -> b.ToString()
                | VoidValue -> "VOID"

    type Function(b:Expression, fA:Name[]) =
            member this.body = b
            member this.formalArgs = fA

    // ENVIRONMENT types: tracks named variables in interpreted programs
    
        type Binding(n: Name, v: Value) =
            member this.name = n
            member this.value = v
        //
        // Contains a given binding and a reference to the context environment.
        // 
        // This class essentially simulates a linked list (each Env consitutes a node
        // and the end of the list is denoted by EMPTY)
        //
        type Environment =
            | EMPTY 
            | Env of Binding * Environment
    
            //
            // Returns a new environment with the given binding and all existing
            // bindings.
            //
            member this.bind (b: Binding):Environment = 
                Env(b, this)

            //
            // Returns the value associated with the given name.
            // 
            member this.lookup (searchName:Name) : Value =
                match this with
                | EMPTY -> failwith("Lookup on " + searchName.name_data + " failed")
                | Env(currBind, refEnv) when (currBind.name.name_data.Equals(searchName.name_data))
                    -> currBind.value
                | Env(currBind, refEnv) -> refEnv.lookup(searchName)


            //
            // Returns a new environment with the replaced binding and all existing
            // bindings (preserves order).
            //
            // E[("x", 5); ("y", 7)].set "x" 7 => E2[("x", 7); ("y", 7)]
            //
            member this.set (setName:Name) (value:Value) : Environment =
                this._set setName value Environment.EMPTY

            // recursively build the new environment
            member private this._set(setName:Name) (value:Value) (newEnv:Environment) =

                match this with
                // invalid case: fail
                | EMPTY -> failwith("Lookup on " + setName.name_data + " failed when setting")

                // set case: form new binding and join with referencing env
                | Env(currBind, refEnv) when (currBind.name.name_data.Equals(setName.name_data))
                    ->  let currEnvWithSet:Environment = (newEnv.bind(Binding(setName, value))).reverse
                        currEnvWithSet.join refEnv

                // copy case: include current binding and referencing env
                | Env (currBind, refEnv) -> refEnv._set setName value (newEnv.bind(currBind))

            //
            // Returns the concatenation of the two environments (preserves order).
            // 
            // E1[b1; b2; EMPTY].join E2[b11; b12; EMPTY] => E3[b1; b2; b11; b12; EMPTY]
            //
            member this.join otherEnv : Environment =
                this._join otherEnv EMPTY

            // recursively build the joined environment by building the reverse of the (other env)
            // followed by the reverse of (this env).
            member private this._join (otherEnv:Environment) (newEnv:Environment) =
                match this, otherEnv with
                // base case: reverse the built environment
                | EMPTY, EMPTY -> newEnv.reverse

                // consume other case: this is exhausted
                | EMPTY, Env(otherBind, otherRef) -> this._join otherRef (newEnv.bind(otherBind))

                // consume this case: this still has bindings
                | Env(thisBind, thisRef), _ -> thisRef._join otherEnv (newEnv.bind(thisBind))

            //
            // Returns an environment with the reverse order of this environment.
            //
            member this.reverse : Environment =
                this._reverse EMPTY

            // recursively build the reverse environment by "prepending" each ref env in order.
            member private this._reverse (newEnv:Environment) =
                match this with
                | EMPTY -> newEnv
                | Env(bind, ref) -> ref._reverse (newEnv.bind(bind))

