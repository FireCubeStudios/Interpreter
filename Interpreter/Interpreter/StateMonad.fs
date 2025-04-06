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

    let random : int stateMonad = SM (fun st -> Ok (State.random st, st))

    let evalState st a =
        match a with
        | Ok (v, state) -> Ok v
        | Error e -> Error e;