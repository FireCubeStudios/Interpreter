module Interpreter.State
    (*
        This module Interpreter.State ontains program state
    *)
    open Result
    open Language
    
    (*
        Checks if variable name v is reserved
        Reserved names: if, then, else, while, declare, print, random, fork, or __result__
    *)
    let reservedVariableName v = 
        match v with
        | "if" | "then" | "else" | "while" | "declare" | "print" | "random" | "fork" | "__result__" -> true
        | _ -> false;;

    (* 
       Checks if variable name v is valid
       - Valid if v starts with a letter or underscore
       - Valid if v contains only letters, numbers, or underscores
    *)
    let validVariableName (v: string) = 
        if System.Char.IsAsciiLetter v[0] || v[0] = '_' then
            String.forall (function c -> System.Char.IsAsciiLetterOrDigit c || c = '_') v
        else    
            false;;
    
    (*
        A "state" type record which contains a Map from strings to integers that contains the variables
        aka basically a Map<VARIABLE_NAME, VARIABLE_VALUE>
    *)
    type state = { variables: Map<string, int> }

    // A function that returns a state with an empty variable environment unit -> state
    let mkState () = { variables = Map.empty }
    
    (*
        A function to declare a variable 'x' with an initial value of 0
        The function takes a variable name 'x' and a state 'st' and returns a "state Result"
        The function returns an Error of "error" type if any of the following conditions are violated:
        - x is a valid variable name
        - x is not a reserved variable name
        - x does not exist in the variables in the state "st"
        If all conditions are valid then a "state Result.Ok" with the variable 'x' = 0 is returned
        Otherwise the corresponding Result.Error is returned with the 'x' variable name as the argument
    *)
    let declare x st = 
        match x with
        | x when Map.containsKey x st.variables -> Error (error.VarAlreadyExists x)
        | x when not(validVariableName x) -> Error (error.InvalidVarName x)
        | x when reservedVariableName x -> Error (error.ReservedName x)
        | _ -> Ok { variables = Map.add x 0 st.variables };;

    (*
        Given a variable name 'x' and a state 'st' return the value of 'x' if it exists in the state environment
        We check if 'x' exists in the state environment by using "Map.containsKey" on the state "st"
        If 'x' exists in "st" we return the value of 'x' in a "Result.Ok" type with the value as the argument
        If 'x' does not exist in the state return "Result.Error" of "error.VarNotDeclared" with 'x' variable name as argument
    *)
    let getVar x st = 
        if Map.containsKey x st.variables then 
            Ok (Map.find x st.variables)
        else 
            Error (error.VarNotDeclared x);;

    (*
        Given a variable name 'x', an int value 'v' and a state 'st' set the value of 'x' to 'v'
        We check if 'x' exists in the state environment by using "Map.containsKey" on the state "st"
        If 'x' exists in the state environment "st" we will first add set the value of 'x' to be 'v' in "st"
        Next we will return the new "st" state environment in a "Result.Ok" type which has the variable 'x' updated to the value 'v'
        If 'x' does not exist in the state return "Result.Error" of "error.VarNotDeclared" with 'x' variable name as argument
    *)
    let setVar x v st =
        if Map.containsKey x st.variables then
            Ok { variables = Map.add x v st.variables }
        else
            Error (error.VarNotDeclared x);;

    let random _ = failwith "not implemented"
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     