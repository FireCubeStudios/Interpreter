module Program

    open Interpreter.Eval
    open Interpreter.StateMonad
    open Interpreter.State
    open Interpreter.JParsec

    let rec parseArgs =
        function
        | []                -> Map.empty
        | name::value::rest -> Map.add name (System.Int32.Parse(value)) (parseArgs rest)
        | _                 -> failwith "Invalid input"


    [<EntryPoint>]
    let main args =
        let m = args.[2..] |> Array.toList |> parseArgs
 
        System.IO.File.ReadAllText(args.[1]) |>
        runProgramParser |>
        Result.map
            (fun (prog, body) ->
                stmntEval body |>
                evalState
                    (mkState
                         (m |> Map.tryFind "--memSize" |> Option.defaultValue 0)
                         (m |> Map.tryFind "--seed")
                         prog)) |>
        printfn "\n\n%A"
        0