module Interpreter.State
    (*
        Contains program state
    *)
    open Result
    open Language
    
    // Checks if variable name v is reserved
    // Reserved names: if, then, else, while, declare, print, random, fork, or __result__
    let reservedVariableName v = 
        match v with
        | "if" | "then" | "else" | "while" | "declare" | "print" | "random" | "fork" | "__result__" -> true
        | _ -> false

    (* 
       Checks if variable name v is valid
       - Valid if v starts with a letter or underscore
       - Valid if v contains only letters, numbers, or underscores
    *)
    let validVariableName (v: string) = 
        if System.Char.IsAsciiLetter v[0] || v[0] = '_' then
            String.forall (function c -> System.Char.IsAsciiLetterOrDigit c || c = '_') v
        else    
            false
    
    type state = 
    | S of string
    | X of int
    | Empty of int
    
    let mkState = state.Empty

    let random _ = failwith "not implemented"
    
    let declare _ = failwith "not implemented"
    
    let getVar _ = failwith "not implemented"
    let setVar _ = failwith "not implemented"
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     