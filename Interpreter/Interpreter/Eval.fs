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
        The result is returned as an "int option":
        - return Some(n) if 'a' is equal to "Num n"
        - return Some(x) if 'a' is equal to "Var v" and the "state" contains the variable 'v'
        - return Some(x + y) if 'a' is equal to Add(x, y), 'x' evaluates to "Some(x)" and 'y' evaluates to "Some(y)"
        - return Some(x * y) if 'a' is equal to Mul(x, y), 'x' evaluates to "Some(x)" and 'y' evaluates to "Some(y)"
        - return Some(x / y) if 'a' is equal to Div(x, y), 'x' evaluates to "Some(x)" and 'y' evaluates to "Some(y)"
          'y' should also not be 0 and '/' = integer division
        - return Some(x % y) if 'a' is equal to Mod(x, y), 'x' evaluates to "Some(x)" and 'y' evaluates to "Some(y)"
          'y' should also not be 0
        - return None otherwise
        Note: I used Option.isSome instead of pattern matching here for finding Some and None option types
    *)
    let rec arithEval a st = 
        match a with
        | Num n -> Some(n)
        | Var v when Map.containsKey v st.variables -> Some(Map.find v st.variables)
        | Add (x, y) when Option.isSome (arithEval x st) && Option.isSome (arithEval y st) -> 
            let x = Option.get (arithEval x st)
            let y = Option.get (arithEval y st)
            Some(x + y)
        | Mul (x, y) when Option.isSome (arithEval x st) && Option.isSome (arithEval y st) -> 
            let x = Option.get (arithEval x st)
            let y = Option.get (arithEval y st)
            Some(x * y)
        | Div (x, y) when Option.isSome (arithEval x st) && Option.isSome (arithEval y st) -> 
            let x = Option.get (arithEval x st)
            let y = Option.get (arithEval y st)
            if y <> 0 then Some(x / y) else None // If y != 0
        | Mod (x, y) when Option.isSome (arithEval x st) && Option.isSome (arithEval y st) -> 
            let x = Option.get (arithEval x st)
            let y = Option.get (arithEval y st)
            if y <> 0 then Some(x % y) else None // If y != 0
        | _ -> None;;

    // Equivalent to arithEval with the use of Option.bind
    let rec arithEval2 a st = 
        match a with
        | Num n -> Some(n)
        | Var v when Map.containsKey v st.variables -> Some(Map.find v st.variables)
        | Add (x, y) -> arithEval2 x st |> Option.bind (fun x -> 
                        arithEval2 y st |> Option.bind (fun y -> Some(x + y)))  
        | Mul (x, y) -> arithEval2 x st |> Option.bind (fun x -> 
                        arithEval2 y st |> Option.bind (fun y -> Some(x * y)))          
        | Div (x, y) -> arithEval2 x st |> Option.bind (fun x -> 
                        arithEval2 y st |> Option.bind (fun y -> if y <> 0 then Some(x / y) else None))  
        | Mod (x, y) -> arithEval2 x st |> Option.bind (fun x -> 
                        arithEval2 y st |> Option.bind (fun y -> if y <> 0 then Some(x % y) else None))  
        | _ -> None;;
    
    (*
        A function which evaluates boolean statements
        It takes a boolean expression 'b' of type "bexpr" defined in Language.fs
        It also takes in a variable state environment "st" of type "state" defined in State.fs
        The boolean expression 'b' is an expression tree which is recursively evaluated and the result is returned
        The result is returned as a "bool option":
        - return Some(true) if 'b' is equal to TT
        - return Some(x = y) if 'b' is equal to Eq(x, y), where 'x' evaluates to "Some(x)" and 'y' evaluates to "Some(y)"
          'x' and 'y' are of type "aexpr"
        - return Some(x < y) if 'b' is equal to Lt(x, y), where 'x' evaluates to "Some(x)" and 'y' evaluates to "Some(y)"
          'x' and 'y' are of type "aexpr"
        - return Some(a && b) if 'b' is equal to Conj(b1, b2), where 'b1' evaluates to "Some(b1)" and 'y' evaluates to "Some(b2)"
          'b1' and 'b2' are of type "bexpr"
        - return Some(not bool) if 'b' is equal to Not(bool), where 'bool' evaluates to "Some(bool)" and is of type "bexpr"
        - return None otherwise
    *)
    let rec boolEval b st =
        match b with
        | TT -> Some(true)
        | Eq(x, y) -> arithEval x st |> Option.bind (fun x -> 
                      arithEval y st |> Option.bind (fun y -> Some(x = y))) 
        | Lt(x, y) -> arithEval x st |> Option.bind (fun x -> 
                      arithEval y st |> Option.bind (fun y -> Some(x < y))) 
        | Conj(b1, b2) -> boolEval b1 st |> Option.bind (fun b1 -> 
                          boolEval b2 st |> Option.bind (fun b2 -> Some(b1 && b2))) 
        | Not(bool) -> boolEval bool st |> Option.bind (fun bool ->  Some(not bool)) 

   (*
        A function which evaluetes statements
        It takes a statement expression 's' of type "stmnt" defined in Language.fs
        It also takes in a variable state environment "st" of type "state" defined in State.fs
        The statement expression 'stmnt' is an expression tree which is recursively evaluated and the result is returned
        The result is returned as a "state option":

        - return Some(st) if 's' is equal to "Skip"

        - return Some(st) if 's' is equal to Declare(v) and 'v' is a variable name of type string
          We return the state variable environment "st" with the variable 'v' declared in "st"
          We do this by using the State.fs "declare" function

        - return Some(st) if 's' is equal to Assign(v, x)
          'v' is a variable name of type string and 'x' is a variable value of type "aexpr"
          We assign a value 'x' to the variable 'v' in the "st" state environment by using the State.fs "setVar" function
          We then return the new state variable environment "st"

        - return Some(st) if 's' is equal to Seq(s1, s2)
          Both "s1" and "s2" are statements of type "stmnt"
          Firstly we evaluate "s1" on the state envionrment "st"
          We will then evaluate "s2" on the resulting state environment "st" from evaluating "s1" earlier
          We then return the new state variable environment "st" 
          The returned result essentially had the statements "s1" and "s2" evaluated sequentially on "st"

        - return Some(st) if 's' is equal to If(b, s1, s2) and 'b' is a boolean expression of type "bexpr"
          "s1" and "s2" are statements of the type "stmnt"
          Firstly we check if the boolean expression 'b' evaluates to "Some(bool)" aka some boolean value instead of "None"
          if "bool" exists and is true or false we do one of the following:
            - If it is true then we evaluate the statement "s1" on the state environment "st"
            - Otherwise we evaluate the statement "s2" on the state environment "st"
          We then return the new state environment "st" as "Some(st)"

        - return Some(st) if 's' is equal to While(b, s) and 'b' is a boolean expression of type "bexpr"
          's' is a statement of the type "stmnt"
          Firstly we check if the boolean expression 'b' evaluates to "Some(bool)" aka some boolean value instead of "None"
          if "bool" exists and is true or false we do one of the following:
            - If it is true then we evaluate the statement 's' on the state environment "st"
              If the result of this is "Some(st)" where "st" is a new state with 's' evaluated then we do the following
                We evaluate the statement "While(b, s)" where 'b' is the boolean expression and 's' is a statement
                'b' and 's' are the same as from the original function parameters
                We return the new state environment variable "st" which has evaluated "While(b, s)"
              Otherwise if the result of 's' evaluated on "st" was "None" then we return "None"
            - Otherwise we return the original state environment variable "st"

        - return None otherwise
    *)
    let rec stmntEval s st = 
        match s with
        | Skip -> Some(st)
        | Declare v -> declare v st
        | Assign(v, x) -> arithEval x st |> Option.bind (fun x -> setVar v x st)
        | Seq(s1, s2) -> stmntEval s1 st |> Option.bind (fun state -> stmntEval s2 state)
        | If(b, s1, s2) -> boolEval b st |> Option.bind (fun bool -> 
            if bool then 
                stmntEval s1 st
            else 
                stmntEval s2 st)
        | While(b, s) -> boolEval b st |> Option.bind (fun bool -> 
            if bool then 
                stmntEval s st |> Option.bind (fun state -> stmntEval (While(b, s)) state)
            else 
                Some(st));;