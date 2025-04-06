// For more information see https://aka.ms/fsharp-console-apps

open Interpreter.Programs
open Interpreter.EvalLegacy
open Interpreter.State

let runProgram prog =
    42 |>
    Some |>
    mkState 10 |>
    stmntEval prog |>
    ignore

// Uncomment the program you want to run

runProgram guessANumber
//runProgram bubbleSort