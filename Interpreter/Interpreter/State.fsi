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
    *)
    type state = { variables: Map<string, int>; memory: memory }

    // A function that returns a state with an empty variable environment and memory of size "memSize"
    val mkState : int -> state
    
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
        A function which given a variable 'x', a memory size "size" and a variable environment state "st"
        Allocates a "size" amount of memory in "st" and returns:
        - Ok st' where st' is the state "st" with the memory allocated and with the variable "x" pointing to the newly allocated memory
        - Returns a "Result.Error" depending on if "alloc" or "setVar" failed
    *)
    val alloc : string -> int -> state -> Result<state, error>
    
    (*
        A function which takes a pointer "ptr", a memory size "size" and a variable environment state "state"
        It frees up memory from the "state" memory starting at the pointer position "ptr" in the "state" memory and returns:
        - Ok st' where st' is the state with the memory freed while the state variables remain the same
        - Returns a "Result.Error" from the "free" function
    *)
    val free : int -> int -> state -> Result<state, error>

    (*
        A function which takes a pointer "ptr", a variable 'v' and a variable environment state "state"
        It sets the variable 'v' at the pointer position "ptr" in the "state" memory and returns:
        - Ok st' where st' is the state with the memory having the variable 'v' at "ptr" while the state variables remain the same
        - Returns a "Result.Error" from the "setMem" function
    *)
    val setMem : int -> int -> state -> Result<state, error>
       
    (*
        A function which takes a pointer "ptr" and a variable environment state "state"
        It gets the variable 'v' at the pointer position "ptr" in the "state" memory and returns:
        - Ok v' where v' is the variable at "ptr" in the "state" memory
        - Returns a "Result.Error" from the "getMem" function
    *)
    val getMem : int -> state -> Result<int, error>
