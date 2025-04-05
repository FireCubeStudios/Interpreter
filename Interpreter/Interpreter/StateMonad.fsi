module Interpreter.StateMonad

    open Language

    type 'a stateMonad
    
    val ret  : 'a -> 'a stateMonad    
    
    val (>>=) : 'a stateMonad -> ('a -> 'b stateMonad) -> 'b stateMonad
    val (>>>=) : 'a stateMonad -> 'b stateMonad -> 'b stateMonad
    
    val fail : error -> 'a stateMonad

    val (>>=) : 'a stateMonad -> ('a -> 'b stateMonad) -> 'b stateMonad
    val (>>>=) : 'a stateMonad -> 'b stateMonad -> 'b stateMonad
