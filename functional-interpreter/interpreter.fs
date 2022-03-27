// F# Interpreter
// 
// This program is capable of "interpreting"
// the basic Expressions listed below.
// Expressions can be composed to create basic programs.
// 
// intrepet syntax: eval([Expression], Environment.EMPTY, Map.empty)
// See demo for an example execution.

namespace functional_interpreter

module interpreter =
    type Operator = PLUS | MINUS | TIMES | DIV

    type Name(n: string) = 
        member this.name_data = n

    // EXPRESSIONS
    type Expression =
        | IntConstant of int32
        | BinOp of Operator * Expression * Expression
        | Let of Name * Expression * Expression
        | Variable of Name
        | Eq of Expression * Expression
        | Neq of Expression * Expression
        | If of Expression * Expression * Expression
        | FunctionDeclaration of Name * Name[] * Expression * Expression
        | FunctionCall of Name * Expression[]
 
    type Value =
        | IntValue of int32
        | BoolValue of bool
        with override this.ToString() =
                match this with
                | IntValue(i) -> i.ToString()
                | BoolValue(b) -> b.ToString()

    type Binding(n: Name, v: Value) =
        member this.name = n
        member this.value = v

    type Environment(b, e) =
        member this.binding = b
        member this.referencingEnvironment = e

        static member EMPTY = Environment(None, None)
 
        member this.bind(b: Binding):Environment = 
            Environment(Some(b), Some(this))

        member this.lookup(name:Name):Value =
            let curr_binding = this.binding.Value
            let curr_ref_env = this.referencingEnvironment.Value
            match curr_binding.name.name_data.Equals(name.name_data) with
            | true -> curr_binding.value
            | false -> curr_ref_env.lookup(name)

    type Function(b:Expression, fA:Name[]) =
            member this.body = b
            member this.formalArgs = fA
 

    // EVALUATION FUNCTION
    // Recursively evaluates the given expression.
    // usage: eval([Expression], Environment.EMPTY, Map.empty)
    // returns: The evaluated value from the expression.
    let rec eval(c:Expression, e:Environment, knownFunctions:Map<string, Function>) =
        match c with

        // constant exp
        | IntConstant(value) -> (IntValue value)

        // binary operator exp
        | BinOp(op, left, right) 
            ->  let l_value = eval(left, e, knownFunctions)
                let r_value = eval(right, e, knownFunctions)

                let l =
                        match l_value with
                        | IntValue(i) -> i
                        | _ -> -1

                let r =
                        match r_value with
                        | IntValue(i) -> i
                        | _ -> -1

                match op with
                | PLUS -> IntValue (l + r)
                | MINUS -> IntValue (l - r)
                | TIMES -> IntValue (l * r)
                | DIV -> IntValue (l / r)  

        // let exp
        | Let(var_name, var_value_exp, body) 
            ->  let var_value = eval(var_value_exp, e, knownFunctions)
                let new_binding = Binding(var_name, var_value)
                let newE = e.bind new_binding
                eval(body, newE, knownFunctions)

        // var exp
        | Variable(var_name) -> e.lookup(var_name)

        // comparison exp (eq)
        | Eq(left, right)
            ->  let l_value = eval(right, e, knownFunctions)
                let r_value = eval(left, e, knownFunctions)

                let l =
                        match l_value with
                        | IntValue(i) -> i
                        | _ -> -1

                let r =
                        match r_value with
                        | IntValue(i) -> i
                        | _ -> -1

                BoolValue(l = r)

        // comparison exp (neq)
        | Neq(left, right)
            ->  let l_value = eval(right, e, knownFunctions)
                let r_value = eval(left, e, knownFunctions)

                let l =
                        match l_value with
                        | IntValue(i) -> i
                        | _ -> -1

                let r =
                        match r_value with
                        | IntValue(i) -> i
                        | _ -> -1

                BoolValue(l <> r)

        // if exp
        | If(cond, thenSide, elseSide)
            ->    // evaluate condition
                let cond_value = eval(cond, e, knownFunctions)
                let execute_thenSide =
                        match cond_value with
                        | BoolValue(b) -> b
                        | _ -> false

                match execute_thenSide with
                | true -> eval(thenSide, e, knownFunctions)
                | false -> eval(elseSide, e, knownFunctions)

        // function declaration exp
        | FunctionDeclaration(name, formalArgs, body, scope)
            ->    
            let newFunction = Function(body, formalArgs)

            // evaluate the scope that this function is defined for
            eval(scope, e, knownFunctions.Add(name.name_data, newFunction))

        // function call exp
        | FunctionCall(name, actualArgs)
            ->
            let calledFunction = knownFunctions.TryFind(name.name_data).Value
            
            // evaluate the actual argument values
            let actualVals = List.ofArray(actualArgs) 
                                |> List.map(fun arg -> eval(arg, e, knownFunctions)) 
                                |> List.toArray

            // place the arg vals in a new environment
            let mutable functionEnv = e
            for ind in 0..actualVals.Length-1 do
                    let curr_name = calledFunction.formalArgs.[ind]
                    let curr_val = actualVals.[ind]
                    functionEnv <- functionEnv.bind(Binding(curr_name, curr_val))

            // evaluate the function with the actual arg vals
            eval(calledFunction.body, functionEnv, knownFunctions)