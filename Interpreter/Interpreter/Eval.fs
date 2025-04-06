module Interpreter.Eval
    (*
        TEMPORARY
    *)
    open Result
    open Language
    open StateMonad

    (* BELOW IS FROM OTHER "FUNCTIONAL" CODE FOLDER RTODO: ADD COMMENTS*)
    let readFromConsole () = System.Console.ReadLine().Trim()
    let tryParseInt (str : string) = System.Int32.TryParse str

    let rec readInt () = 
        let input = readFromConsole()
        match tryParseInt input with
        | (true, n) -> n
        | (false, n) -> printfn "%s is not an integer" input // Fixes bug "The type 'string' is not compatible with the type 'Printf.TextWriterFormat<'a>'"
                        readInt ();;
    (* end of region*)
    
    let rec arithEval a = 
        match a with
        | Num n -> ret n
        | Var v -> getVar v
        | Add (x, y) -> 
                arithEval x >>= fun x ->  
                arithEval y >>= fun y -> ret (x + y)
        | Mul (x, y) ->  
                arithEval x >>= fun x ->  
                arithEval y >>= fun y -> ret (x * y)
        | Div (x, y) ->  
                arithEval x >>= fun x ->  
                arithEval y >>= fun y ->  
                    if y <> 0 then ret (x / y) else fail error.DivisionByZero // y <> 0 == y != 0    
        | Mod (x, y) ->  
                arithEval x >>= fun x ->  
                arithEval y >>= fun y ->  
                    if y <> 0 then ret (x % y) else fail error.DivisionByZero // y <> 0 == y != 0
        | MemRead e1 ->
                arithEval e1 >>= fun ptr ->  
                getMem ptr >>= fun x -> ret x
        | Random -> random
        | Read -> ret(readInt())
        | Cond (b, a1, a2) ->
                boolEval b >>= fun x ->  
                if x = true then 
                    arithEval a1
                else
                    arithEval a2
        | FunctionCall(f, list) -> failwith "not implemented"
    and boolEval b =
        match b with
        | TT -> ret true
        | Eq(x, y) -> arithEval x >>= fun x -> 
                      arithEval y >>= fun y -> ret (x = y)
        | Lt(x, y) -> arithEval x >>= fun x -> 
                      arithEval y >>= fun y -> ret (x < y) 
        | Conj(b1, b2) -> 
                      boolEval b1 >>= fun x -> 
                      boolEval b2 >>= fun y -> ret (x && y) 
        | Not(bool) -> boolEval bool >>= fun bool -> ret(not bool);;

    // Equivalent to arithEval with the use of Result.bind
   (* let rec arithEval2 a st : Result<int, error> = 
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
                        arithEval2 y st |> Result.bind (fun y -> if y <> 0 then Ok (x % y) else Error error.DivisionByZero))
        | MemRead e1 -> arithEval2 e1 st |> Result.bind (fun ptr -> getMem ptr st)
        | Random -> Ok (random st)
        | Read -> Ok (readInt())
        | Cond (b, a1, a2) -> boolEval b st |> Result.bind(fun x -> if x = true then arithEval2 a1 st else arithEval2 a2 st);;
        *)


    //TODO V4 COMMENTING FOR BELOW CODE
    let split (s1 : string) (s2 : string) = s2 |> s1.Split |> Array.toList // split a string s1 from all occurences of s2 like split "ababc" "b"

    let rec mergeStrings es s =
        match es with
        | [] -> ret ()
        | a::es ->  arithEval a >>= fun x -> ret ();;
    // END REGION

    let rec stmntEval s = 
        match s with
        | Skip -> ret ()
        | Declare v -> declare v
        | Assign(v, x) -> arithEval x >>= fun x -> setVar v x
        | Seq(s1, s2) -> stmntEval s1 >>= fun state -> stmntEval s2
        | If(b, s1, s2) -> boolEval b >>= fun bool -> if bool then stmntEval s1 else stmntEval s2
        | While(b, s) -> boolEval b >>= fun bool ->
                         if bool then
                            stmntEval s >>= fun state -> stmntEval (While(b, s))
                         else
                            ret ()
        | Alloc(x, e) -> arithEval e >>= fun size -> alloc x size
        | Free(e1, e2) -> arithEval e1 >>= fun ptr -> arithEval e2 >>= fun size -> free ptr size
        | MemWrite(e1, e2) -> arithEval e1 >>= fun ptr -> arithEval e2  >>= fun v -> setMem ptr v
        | Print(es, s) -> mergeStrings es s
        | Return x -> failwith "not implemented";; // if result.Chars == 0 then fail (error.IllFormedPrint (s, [0]));; unfinished
        

