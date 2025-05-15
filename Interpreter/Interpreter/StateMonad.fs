module Interpreter.StateMonad

    open State
    open Language

    type 'a stateMonad = SM of (state -> Result<'a * state, error>)
        
    let ret x = SM (fun st -> Ok(x, st))
    let fail err = SM (fun _ -> Error err)

    let bind (SM f) g =
        SM (fun st ->
            match f st with
            | Ok (x, st') -> let (SM h) = g x in h st'
            | Error err   -> Error err) 

    let (>>=) a f = bind a f
    let (>>>=) a b = a >>= (fun _ -> b)

    (* Below are functions for the Monad *)

    let declare str =
        SM (fun st ->
            match State.declare str st with
            | Ok state -> Ok((), state)
            | Error e -> Error e)

    let setVar str v =
        SM (fun st ->
            match State.setVar str v st with
            | Ok state -> Ok((), state)
            | Error e -> Error e)

    let getVar str =
        SM (fun st ->
            match State.getVar str st with
            | Ok var -> Ok(var, st)
            | Error e -> Error e)

    let alloc str size =
        SM (fun st ->
            match State.alloc str size st with
            | Ok state -> Ok((), state)
            | Error e -> Error e)

    let free ptr size =
        SM (fun st ->
            match State.free ptr size st with
            | Ok state -> Ok((), state)
            | Error e -> Error e)

    let setMem ptr v =
        SM (fun st ->
            match State.setMem ptr v st with
            | Ok state -> Ok((), state)
            | Error e -> Error e)

    let getMem ptr =
        SM (fun st ->
            match State.getMem ptr st with
            | Ok memory -> Ok(memory, st)
            | Error e -> Error e)

    type random = int stateMonad
    let random = SM (fun st -> Ok (State.random st, st))

    let evalState st (SM f) =
       match f st with
        | Ok (v, _) -> Ok v
        | Error e -> Error e;

    // new todo comments
    let fork (ss: unit stateMonad list) : unit stateMonad =
        SM (fun st ->
            let results = List.map (fun m -> evalState st m) ss
            if List.forall Result.isOk results then
                Ok((), st)
            else
                Error error.DivisionByZero// TEMPORARY, i did yellow exercises earlier but now i want to do green
        )