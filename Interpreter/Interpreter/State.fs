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
        The function takes a variable name 'x' and a state 'st' and returns a "state option"
        The "state option" returns "None" if any of the following conditions are violated:
        - x is a valid variable name
        - x is not a reserved variable name
        - x does not exist in the variables in the state "st"
        If all conditions are valid then a "state option" with the variable 'x' = 0 is returned
    *)
    let declare x st = 
        if not(Map.containsKey x st.variables) && validVariableName x && not(reservedVariableName x) then
            Some({ variables = Map.add x 0 st.variables })
        else
            None;;

    (*
        Given a variable name 'x' and a state 'st' return the value of 'x' if it exists in the state
        If it does not exist in the state return None 
        Otherwise return the value in a "Some" int option
    *)
    let getVar x st = Map.tryFind x st.variables

    (*
        Given a variable name 'x', an int value 'v' and a state 'st' set the value of 'x' to 'v'
        If 'x' does not exist in the state return None 
        Otherwise return the "state option" which has the variable 'x' updated to the value 'v'
    *)
    let setVar x v st =
        if Map.containsKey x st.variables then
            Some({ variables = Map.add x v st.variables })
        else
            None;;

    let random _ = failwith "not implemented"
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     