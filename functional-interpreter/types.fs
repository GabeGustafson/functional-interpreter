namespace functional_interpreter

// this module defines the types that the interpreter must handle
module types =
    // FUNDAMENTAL TYPES

    type Operator = PLUS | MINUS | TIMES | DIV

    type Name(n: string) = 
        member this.name_data = n


    // STATEMENTS

    // Expressions must evaluate to a Value and do not directly change program state
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
    and Statement = // Statements may evaluate to a Value and may directly change program state
        | Expr of Expression
        | Seq of Statement[]
        | SetVar of Name * Expression


    // INTREPTER TYPES

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
    
        type Environment(b, e) =
            member this.binding = b
            member this.referencingEnvironment = e
    
            // Note: binding and ref env are optional, but may only be None in the EMPTY environment
            static member EMPTY = Environment(None, None)
    
            //
            // Returns a new environment with the given binding and all existing
            // bindings.
            //
            member this.bind (b: Binding):Environment = 
                Environment(Some(b), Some(this))

            //
            // Returns the value associated with the given name.
            // 
            member this.lookup (name:Name) : Value =
                let curr_binding = this.binding.Value
                let curr_ref_env = this.referencingEnvironment.Value

                if this.Equals(Environment.EMPTY) then
                    failwith("Lookup on " + name.name_data + " failed")
                else if curr_binding.name.name_data.Equals(name.name_data) then
                    curr_binding.value
                else
                    curr_ref_env.lookup(name)

            //
            // Returns a new environment with the replaced binding and all existing
            // bindings.
            //
            member this.set (name:Name) (value:Value) : Environment =
                this._set name value Environment.EMPTY

            member private this._set(name:Name) (value:Value) (newEnv:Environment) =
                let curr_binding = this.binding.Value
                let curr_ref_env = this.referencingEnvironment.Value

                // recursively build the new environment
                if this.Equals(Environment.EMPTY) then
                    failwith("Lookup on " + name.name_data + " failed when setting")
                // set case: form new binding and join with referencing env
                else if curr_binding.name.name_data.Equals(name.name_data) then
                    // reverse the reversed environment
                    Environment.EMPTY.join ((newEnv.bind(Binding(name, value))).join curr_ref_env)
                // copy case: include current binding and referencing env
                else
                    curr_ref_env._set name value (newEnv.bind(curr_binding))

            //
            // Returns the union of the two environments (reverses order of other environment).
            //
            member this.join otherEnv : Environment =
                this._join otherEnv this

            member private this._join (otherEnv:Environment) (currEnv:Environment) =
                // base case: other environment is exhausted
                if otherEnv.Equals(Environment.EMPTY) then
                    currEnv
                else // inductive case: continue to build new Environment
                    this._join (otherEnv.referencingEnvironment.Value) (currEnv.bind(otherEnv.binding.Value))
                    // NOTE: .Value is necessary because these are optional types in the environment