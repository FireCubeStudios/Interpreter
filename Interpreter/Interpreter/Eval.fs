module Interpreter.Eval
    (*
        The interpreter code that evaluates arithmetic statements and boolean statements
    *)
    open Result
    open Language
    open State
    
    (*
        A function which evaluates arithmetic statements
        It takes an arithmetic expression 'a' of type "aexpr" defined in Language.fs
        It also takes in a variable state environment "st" of type "state" defined in State.fs
        The arithmetic expression 'a' is an expression tree which is recursively evaluated and the result is returned
        The result is returned as a "Result<int, error>" type where "error" is defined in Language.fs:
        - return Result.Ok(n) if 'a' is equal to "Num n"
        - return Result.Ok(x) if 'a' is equal to "Var v" and the "state" contains the variable 'v'
        - return Result.Ok(x + y) if 'a' is equal to Add(x, y), 'x' evaluates to "Result.Ok(x)" and 'y' evaluates to "Result.Ok(y)"
        - return Result.Ok(x * y) if 'a' is equal to Mul(x, y), 'x' evaluates to "Result.Ok(x)" and 'y' evaluates to "Result.Ok(y)"
        - return Result.Ok(x / y) if 'a' is equal to Div(x, y), 'x' evaluates to "Result.Ok(x)" and 'y' evaluates to "Result.Ok(y)"
          'y' should also not be 0 and if it is then we return "Result.Error" of type "error.DivisionByZero"
          '/' = integer division
        - return Result.Ok(x % y) if 'a' is equal to Mod(x, y), 'x' evaluates to "Result.Ok(x)" and 'y' evaluates to "Result.Ok(y)"
          'y' should also not be 0 and if it is then we return "Result.Error" of type "error.DivisionByZero"
        - otherwise we propagate the "Result.Error" type from recursive calls or other functions that returned error
    *)
    let rec arithEval a st = 
        match a with
        | Num n -> Ok n
        | Var v -> getVar v st
        | Add (x, y) -> 
            match arithEval x st with
            | Ok x -> match arithEval y st with
                      | Ok y -> Ok (x + y)
                      | Error e -> Error e
            | Error e -> Error e
        | Mul (x, y) -> 
            match arithEval x st with
            | Ok x -> match arithEval y st with
                      | Ok y -> Ok (x * y)
                      | Error e -> Error e
            | Error e -> Error e
        | Div (x, y) -> 
            match arithEval x st with
            | Ok x -> match arithEval y st with
                      | Ok y -> if y <> 0 then Ok (x / y) else Error error.DivisionByZero // y <> 0 == y != 0
                      | Error e -> Error e
            | Error e -> Error e
        | Mod (x, y) -> 
            match arithEval x st with
            | Ok x -> match arithEval y st with
                      | Ok y -> if y <> 0 then Ok (x % y) else Error error.DivisionByZero // y <> 0 == y != 0
                      | Error e -> Error e
            | Error e -> Error e;;

    // Equivalent to arithEval with the use of Result.bind
    let rec arithEval2 a st : Result<int, error> = 
        match a with
        | Num n -> Ok n
        | Var v -> getVar v st
        | Add (x, y) -> arithEval2 x st |> Result.bind (fun x -> 
                        arithEval2 y st |> Result.bind (fun y -> Ok (x + y)))  
        | Mul (x, y) -> arithEval2 x st |> Result.bind (fun x -> 
                        arithEval2 y st |> Result.bind (fun y -> Ok (x * y)))          
        | Div (x, y) -> arithEval2 x st |> Result.bind (fun x -> 
                        arithEval2 y st |> Result.bind (fun y -> if y <> 0 then Ok (x / y) else Error error.DivisionByZero))  
        | Mod (x, y) -> arithEval2 x st |> Result.bind (fun x -> 
                        arithEval2 y st |> Result.bind (fun y -> if y <> 0 then Ok (x % y) else Error error.DivisionByZero));;
    
    (*
        A function which evaluates boolean statements
        It takes a boolean expression 'b' of type "bexpr" defined in Language.fs
        It also takes in a variable state environment "st" of type "state" defined in State.fs
        The boolean expression 'b' is an expression tree which is recursively evaluated and the result is returned
        The result is returned as a "Result<bool, error>" type where "error" is defined in Language.fs:
        - return Result.Ok(true) if 'b' is equal to TT
        - return Result.Ok(x = y) if 'b' is equal to Eq(x, y), where 'x' evaluates to "Result.Ok(x)" and 'y' evaluates to "Result.Ok(y)"
          'x' and 'y' are of type "aexpr"
        - return Result.Ok(x < y) if 'b' is equal to Lt(x, y), where 'x' evaluates to "Result.Ok(x)" and 'y' evaluates to "Result.Ok(y)"
          'x' and 'y' are of type "aexpr"
        - return Result.Ok(a && b) if 'b' is equal to Conj(b1, b2), where 'b1' evaluates to "Result.Ok(b1)" and 'y' evaluates to "Result.Ok(b2)"
          'b1' and 'b2' are of type "bexpr"
        - return Result.Ok(not bool) if 'b' is equal to Not(bool), where 'bool' evaluates to "Result.Ok(bool)" and is of type "bexpr"
        - otherwise we propagate the "Result.Error" type from "arithEval" or recursive "boolEval" calls
    *)
    let rec boolEval b st =
        match b with
        | TT -> Ok true
        | Eq(x, y) -> arithEval x st |> Result.bind (fun x -> 
                      arithEval y st |> Result.bind (fun y -> Ok (x = y))) 
        | Lt(x, y) -> arithEval x st |> Result.bind (fun x -> 
                      arithEval y st |> Result.bind (fun y -> Ok (x < y))) 
        | Conj(b1, b2) -> boolEval b1 st |> Result.bind (fun b1 -> 
                          boolEval b2 st |> Result.bind (fun b2 -> Ok (b1 && b2))) 
        | Not(bool) -> boolEval bool st |> Result.bind (fun bool ->  Ok (not bool)) 

   (*
        A function which evaluetes statements
        It takes a statement expression 's' of type "stmnt" defined in Language.fs
        It also takes in a variable state environment "st" of type "state" defined in State.fs
        The statement expression 'stmnt' is an expression tree which is recursively evaluated and the result is returned
        The result is returned as a "Result<state, error>" type where "error" is defined in Language.fs:

        - return Result.Ok(st) if 's' is equal to "Skip"

        - return Result.Ok(st) if 's' is equal to Declare(v) and 'v' is a variable name of type string
          We return the state variable environment "st" with the variable 'v' declared in "st"
          We do this by using the State.fs "declare" function

        - return Result.Ok(st) if 's' is equal to Assign(v, x)
          'v' is a variable name of type string and 'x' is a variable value of type "aexpr"
          We assign a value 'x' to the variable 'v' in the "st" state environment by using the State.fs "setVar" function
          We then return the new state variable environment "st"

        - return Result.Ok(st) if 's' is equal to Seq(s1, s2)
          Both "s1" and "s2" are statements of type "stmnt"
          Firstly we evaluate "s1" on the state envionrment "st"
          We will then evaluate "s2" on the resulting state environment "st" from evaluating "s1" earlier
          We then return the new state variable environment "st" 
          The returned result essentially had the statements "s1" and "s2" evaluated sequentially on "st"

        - return Result.Ok(st) if 's' is equal to If(b, s1, s2) and 'b' is a boolean expression of type "bexpr"
          "s1" and "s2" are statements of the type "stmnt"
          Firstly we check if the boolean expression 'b' evaluates to "Result.Ok(bool)" instead of a "Result.Error"
          if "bool" exists and is true or false we do one of the following:
            - If it is true then we evaluate the statement "s1" on the state environment "st"
            - Otherwise we evaluate the statement "s2" on the state environment "st"
          We then return the new state environment "st" as "Result.Ok(st)"

        - return Result.Ok(st) if 's' is equal to While(b, s) and 'b' is a boolean expression of type "bexpr"
          's' is a statement of the type "stmnt"
          Firstly we check if the boolean expression 'b' evaluates to "Result.Ok(bool)" instead of a "Result.Error"
          if "bool" exists and is true or false we do one of the following:
            - If it is true then we evaluate the statement 's' on the state environment "st"
              If the result of this is "Result.Ok(st)" where "st" is a new state with 's' evaluated then we do the following
                We evaluate the statement "While(b, s)" where 'b' is the boolean expression and 's' is a statement
                'b' and 's' are the same as from the original function parameters
                We return the new state environment variable "st" which has evaluated "While(b, s)"
              Otherwise if the result of 's' evaluated on "st" was of type "Result.Error" then we return that error
            - Otherwise we return the original state environment variable "st"

        - otherwise we propagate the "Result.Error" type from "declare", "setVar", "arithEval", "boolEval" and recursive "stmntEval" calls
    *)
    let rec stmntEval s st = 
        match s with
        | Skip -> Ok st
        | Declare v -> declare v st
        | Assign(v, x) -> arithEval x st |> Result.bind (fun x -> setVar v x st)
        | Seq(s1, s2) -> stmntEval s1 st |> Result.bind (fun state -> stmntEval s2 state)
        | If(b, s1, s2) -> boolEval b st |> Result.bind (fun bool -> 
            if bool then 
                stmntEval s1 st
            else 
                stmntEval s2 st)
        | While(b, s) -> boolEval b st |> Result.bind (fun bool -> 
            if bool then 
                stmntEval s st |> Result.bind (fun state -> stmntEval (While(b, s)) state)
            else 
                Ok st);;