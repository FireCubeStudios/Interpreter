module Interpreter.State
    (*
        This module Interpreter.State ontains program state
    *)
    open Memory
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
        A "state" type record which represents the current program state
        It contains a Map from strings to integers that contains the variables e.g Map<VARIABLE_NAME, VARIABLE_VALUE>
        The state also contains a "memory" record which represents the current program memory
    *)
    type state = { variables: Map<string, int>; memory: memory }

    // A function that returns a state with an empty variable environment and memory of size "memSize"
    let mkState memSize = { variables = Map.empty; memory = empty memSize }
    
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
        | _ -> Ok { variables = Map.add x 0 st.variables; memory = st.memory };;

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
            Ok { variables = Map.add x v st.variables; memory = st.memory }
        else
            Error (error.VarNotDeclared x);;

    let random _ = failwith "not implemented"
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"  
    
    (*
        Below we introduce new methods for working with memory using functions from Memory.fs
    *)

    (* 
        A function which given a variable 'x', a memory size "size" and a variable environment state "st"
        Allocates a "size" amount of memory in "st" and returns:
        - Ok st' where st' is the state "st" with the memory allocated and with the variable "x" pointing to the newly allocated memory
        - Returns a "Result.Error" depending on if "alloc" or "setVar" failed
    *)
    let alloc x size st =
        match alloc size st.memory with
        | Ok (mem', ptr) -> 
            match setVar x ptr st with
            | Ok st' -> Ok { st' with memory = mem' }  // Update memory in new state
            | Error e -> Error e  // Variable assignment failed
        | Error e -> Error e  // Memory allocation failed
    
    (*
        A function which takes a pointer "ptr", a memory size "size" and a variable environment state "state"
        It frees up memory from the "state" memory starting at the pointer position "ptr" in the "state" memory and returns:
        - Ok st' where st' is the state with the memory freed while the state variables remain the same
        - Returns a "Result.Error" from the "free" function
    *)
    let free ptr size state = 
        let memory = free ptr size state.memory
        match memory with
        | Ok memory -> Ok { variables = state.variables; memory = memory }
        | Error e -> Error e

    (*
        A function which takes a pointer "ptr", a variable 'v' and a variable environment state "state"
        It sets the variable 'v' at the pointer position "ptr" in the "state" memory and returns:
        - Ok st' where st' is the state with the memory having the variable 'v' at "ptr" while the state variables remain the same
        - Returns a "Result.Error" from the "setMem" function
    *)
    let setMem ptr v state = 
        let memory = setMem ptr v state.memory
        match memory with
        | Ok memory -> Ok { variables = state.variables; memory = memory }
        | Error e -> Error e
       
    (*
        A function which takes a pointer "ptr" and a variable environment state "state"
        It gets the variable 'v' at the pointer position "ptr" in the "state" memory and returns:
        - Ok v' where v' is the variable at "ptr" in the "state" memory
        - Returns a "Result.Error" from the "getMem" function
    *)
    let getMem ptr state = getMem ptr state.memory