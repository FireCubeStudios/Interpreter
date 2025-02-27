module Interpreter.Eval
    (*
        The interpreter code that evaluates arithmetic statements and boolean statements
    *)
    open Result
    open Language
    open State
    
    (*
        A function which evaluetes arithmetic statements
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
        A function which evaluetes boolean statements
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


    let stmntEval s st = failwith "not implemented"