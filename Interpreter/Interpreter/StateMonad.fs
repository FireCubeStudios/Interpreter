module Interpreter.StateMonad

    open State
    open Language

    type 'a stateMonad = SM of (state -> Result<'a * state, error>)
        
    let ret x      = SM (fun st -> Ok(x, st))
    let fail err= SM (fun _ -> Error err)
    
    let bind (SM f) g =
        SM (fun st ->
            match f st with
            | Ok (x, st') -> let (SM h) = g x in h st'
            | Error err   -> Error err) 