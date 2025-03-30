module Interpreter.State
    (*
        This signature file contains signatures for State.fs that manages program state
    *)
    open Memory
    open Language
    
    (*
        A "state" type record which contains a Map from strings to integers that contains the variables
        aka basically a Map<VARIABLE_NAME, VARIABLE_VALUE> for the variables
        This represnts the current program state
        NEW: Added memory with the "memory" type from Memory.fs
        
        TODO: V4
    *)
    type state = { variables: Map<string, int>; memory: memory; rng: System.Random }

    (*
        A function that returns a state with an empty variable environment and memory of size "memSize"

        TODO: V4 COMMENTS
    *)
    val mkState : int -> int option -> state

    (*
    TODO: V4
    *)
    val random : state -> int
    
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
    val declare : string -> state -> Result<state, error>

    (*
        Given a variable name 'x' and a state 'st' return the value of 'x' if it exists in the state environment
        We check if 'x' exists in the state environment by using "Map.containsKey" on the state "st"
        If 'x' exists in "st" we return the value of 'x' in a "Result.Ok" type with the value as the argument
        If 'x' does not exist in the state return "Result.Error" of "error.VarNotDeclared" with 'x' variable name as argument
    *)
    val getVar : string -> state -> Result<int, error>

    (*
        Given a variable name 'x', an int value 'v' and a state 'st' set the value of 'x' to 'v'
        We check if 'x' exists in the state environment by using "Map.containsKey" on the state "st"
        If 'x' exists in the state environment "st" we will first add set the value of 'x' to be 'v' in "st"
        Next we will return the new "st" state environment in a "Result.Ok" type which has the variable 'x' updated to the value 'v'
        If 'x' does not exist in the state return "Result.Error" of "error.VarNotDeclared" with 'x' variable name as argument
    *)
    val setVar : string -> int -> state -> Result<state, error>

    (*
        Below we introduce new methods for working with memory using functions from Memory.fs
    *)

    (*
    *)
    val alloc : string -> int -> state -> Result<state, error>
    
    (*
    *)
    val free : int -> int -> state -> Result<state, error>

    (*
    *)
    val setMem : int -> int -> state -> Result<state, error>
       
    (*
    *)
    val getMem : int -> state -> Result<int, error>
