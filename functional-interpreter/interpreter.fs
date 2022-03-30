// F# Interpreter
// 
// This program is capable of "interpreting"
// the Statements listed below.
// Expressions can be composed to create basic programs.
// 
// intrepet syntax: eval([Expression], Environment.EMPTY, Map.empty)
// See demo for an example execution.

namespace functional_interpreter

open types

module interpreter =

    // ENVIRONMENT types: tracks named variables in interpreted programs

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

    //
    // helper function: returns the environment with the arguments bound to it
    //
    let rec private applyArgs argNames argVals curr_env:Environment =
        match argNames, argVals with
        | [], [] -> curr_env
        | name::remainNames, value::remainVals -> applyArgs remainNames remainVals ( curr_env.bind(Binding(name, value)) )
        | _, _ -> failwith("Incorrect number of arguments in function call")


    //
    // helper functions: unpacks a value, returning the underlying data type.
    // Fails if the expected type is not found.
    //
    let private unpackIntValue value context =
        match value with
        | IntValue(i) -> i
        | _ -> failwith("Expected int value in " + context)

    let private unpackBoolValue value context =
        match value with
        | BoolValue(b) -> b
        | _ -> failwith("Expected bool value in " + context)
        
    //
    // RECURSIVE EVALUATION FUNCTION
    //
    // Recursively evaluates the given expression.
    // usage: eval([Expression], Environment.EMPTY, Map.empty)
    // returns: The evaluated value from the expression.
    //
    let rec private eval (c:Statement) (e:Environment) (knownFunctions:Map<string, Function>) =
        match c with
        | Expr(exp) -> evalExpr exp e knownFunctions
        | Seq(statements) -> VoidValue
        | SetVar(name, valExpr) -> VoidValue
            
    and private evalExpr (exp:Expression) (e:Environment) (knownFunctions:Map<string, Function>) =
        match exp with
        // constant exp
        | IntConstant(value) -> (IntValue value)
        
        // binary operator exp
        | BinOp(op, left, right) 
            ->  let l_value = evalExpr left e knownFunctions
                let r_value = evalExpr right e knownFunctions
        
                let l = unpackIntValue l_value "binary operation"
                let r = unpackIntValue r_value "binary operation"
        
                match op with
                | PLUS -> IntValue (l + r)
                | MINUS -> IntValue (l - r)
                | TIMES -> IntValue (l * r)
                | DIV -> IntValue (l / r)  
        
        // let exp
        | Let(var_name, var_value_exp, body) 
            ->  let var_value = evalExpr var_value_exp e knownFunctions
                let newE = e.bind (Binding(var_name, var_value))
                eval body newE knownFunctions
        
        // var exp
        | Variable(var_name) -> e.lookup(var_name)
        
        // comparison exp (eq)
        | Eq(left, right)
            ->  let l_value = evalExpr right e knownFunctions
                let r_value = evalExpr left e knownFunctions
        
                let l = unpackIntValue l_value "equality comparison"        
                let r = unpackIntValue r_value "equality comparison"
        
                BoolValue(l = r)
        
        // comparison exp (neq)
        | Neq(left, right)
            ->  let l_value = evalExpr right e knownFunctions
                let r_value = evalExpr left e knownFunctions
        
                let l = unpackBoolValue l_value "inequality comparison"
                let r = unpackBoolValue r_value "inequality comparison"
        
                BoolValue(l <> r)
        
        // if exp
        | If(cond, thenSide, elseSide)
            ->  // evaluate condition
                let cond_value = evalExpr cond e knownFunctions
                let cond = unpackBoolValue cond_value "if condition"
        
                if cond then
                    eval thenSide e knownFunctions
                else
                    eval elseSide e knownFunctions
        
        // function declaration exp
        | FunctionDeclaration(name, formalArgs, body, scope)
            ->    
            let newFunction = Function(body, formalArgs)
        
            // evaluate the scope that this function is defined for
            evalExpr scope e (knownFunctions.Add(name.name_data, newFunction))
        
        // function call exp
        | FunctionCall(name, argSupplied)
            ->
            let calledFunction = knownFunctions.TryFind(name.name_data).Value
                    
            // evaluate the actual argument values
            let argNames = List.ofArray(calledFunction.formalArgs)
        
            let argVals = List.ofArray(argSupplied) 
                                |> List.map(fun argExpr -> evalExpr argExpr e knownFunctions)
        
            // place the computed arg vals in a new environment
            let functionEnv = applyArgs argNames argVals e
        
            // evaluate the function with the new environment that has bound the arguments to their values
            evalExpr calledFunction.body functionEnv knownFunctions

    // API EVALUATION FUNCTION
    let evaluate (c:Statement) =
        eval c Environment.EMPTY Map.empty