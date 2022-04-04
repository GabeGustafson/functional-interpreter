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

    //
    // helper functions: Unpacks a value, returning the underlying data type.
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
    // Recursively evaluates the given statement.
    // initial usage: eval([Statement], Environment.EMPTY, Map.empty)
    // returns: The Value and new environment from evaluating the statement.
    //
    let rec private eval (c:Statement) (e:Environment) (knownFunctions:Map<string, Function>) : (Value * Environment) =
        match c with
        // expression wrapper
        | Expr(exp) -> evalExpr exp e knownFunctions // expressions do not change the environment

        // sequential statements
        | Seq(statements)
            ->  evalSeq statements e knownFunctions
        | SetVar(name, valExpr)
            ->  let valEvaluated, e2 = evalExpr valExpr e knownFunctions
                (VoidValue, e2.set name valEvaluated)
            
    and private evalExpr (exp:Expression) (e:Environment) (knownFunctions:Map<string, Function>) : (Value * Environment) =
        match exp with
        // constant exp
        | IntConstant(value) -> (IntValue value, e)
        
        // binary operator exp
        | BinOp(op, left, right) 
            ->  let l_value, e2 = evalExpr left e knownFunctions
                let r_value, e3 = evalExpr right e2 knownFunctions
        
                let l = unpackIntValue l_value "binary operation"
                let r = unpackIntValue r_value "binary operation"
        
                let resultValue =
                    match op with
                    | PLUS -> IntValue (l + r)
                    | MINUS -> IntValue (l - r)
                    | TIMES -> IntValue (l * r)
                    | DIV -> IntValue (l / r)

                (resultValue, e3)
        
        // let exp
        | Let(var_name, var_value_exp, body) 
            ->  let var_value, e2 = evalExpr var_value_exp e knownFunctions
                let e3 = e2.bind (Binding(var_name, var_value))
                let evalValue, e4 = eval body e3 knownFunctions

                // return the (potentially modified) environment without the let variable
                let e4Ref =
                    match e4 with
                    | EMPTY -> EMPTY
                    | Env(_, ref) -> ref

                (evalValue, e4Ref)
        
        // var exp
        | Variable(var_name) -> e.lookup var_name , e
        
        // comparison exp (eq)
        | Eq(left, right)
            ->  let l_value, e2 = evalExpr right e knownFunctions
                let r_value, e3 = evalExpr left e2 knownFunctions
        
                let l = unpackIntValue l_value "equality comparison"        
                let r = unpackIntValue r_value "equality comparison"
        
                BoolValue(l = r), e3
        
        // comparison exp (neq)
        | Neq(left, right)
            ->  let l_value, e2 = evalExpr right e knownFunctions
                let r_value, e3 = evalExpr left e2 knownFunctions
        
                let l = unpackBoolValue l_value "inequality comparison"
                let r = unpackBoolValue r_value "inequality comparison"
        
                BoolValue(l <> r), e3
        
        // if exp
        | If(cond, thenSide, elseSide)
            ->  // evaluate condition
                let cond_value, e2 = evalExpr cond e knownFunctions
                let cond = unpackBoolValue cond_value "if condition"
        
                if cond then
                    eval thenSide e2 knownFunctions
                else
                    eval elseSide e2 knownFunctions
        
        // function declaration exp
        | FunctionDeclaration(name, formalArgs, body, scope)
            ->    
            let newFunction = Function(body, formalArgs)
        
            // evaluate the scope that this function is defined for
            evalExpr scope e (knownFunctions.Add(name.name_data, newFunction))
        
        // function call exp
        | FunctionCall(name, argSupplied)
            ->
            // find and validate the called function
            let calledFunctionSome = knownFunctions.TryFind(name.name_data)
            if calledFunctionSome.IsNone then
                failwith("Failed lookup on function: " + name.name_data)
            let calledFunction = calledFunctionSome.Value

            // form lists for the argument expressions
            let argNames = List.ofArray(calledFunction.formalArgs)
            let argExprs = List.ofArray(argSupplied) 
        
            // eval and place the arg vals in a new environment
            let functionEnv = applyArgs argNames argExprs knownFunctions e
        
            // evaluate the function with the new environment that has bound the arguments to their values
            evalExpr calledFunction.body functionEnv knownFunctions
    //
    // helper function: Returns the environment with the arguments
    // evaluated and bound to it.
    //
    and private applyArgs argNames argExprs knownFunctions currEnv:Environment =
        match argNames, argExprs with
        | [], [] -> currEnv
        | argName::remainNames, argExpr::remainingExprs ->
            let argVal, newEnv = evalExpr argExpr currEnv knownFunctions
            applyArgs remainNames remainingExprs knownFunctions (newEnv.bind(Binding(argName, argVal)) )
        | _, _ -> failwith("Incorrect number of arguments in function call")
    //
    // helper function: Recursively evaluates a sequential statement to its last
    // evaluated statement's value.
    //
    and private evalSeq (seq:List<Statement>) (e:Environment) (knownFunctions:Map<string, Function>) =
        match seq with
        | [] -> VoidValue, e
        | c::[] -> eval c e knownFunctions
        | c::seq_tail
            -> let _, e2 = eval c e knownFunctions
               evalSeq seq_tail e2 knownFunctions

    // API EVALUATION FUNCTION
    let evaluate (c:Statement) =
        fst(eval c Environment.EMPTY Map.empty)