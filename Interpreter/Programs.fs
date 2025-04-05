module Interpreter.Programs
    (*
        Example programs
    *)
    open Interpreter.Language
    
    let factorial x =
        Seq(Declare "result",
            Seq(Declare "x",
                Seq(Assign("result", Num 1),
                    Seq(Assign("x", Num x),
                        While(Lt(Num 0, Var "x"),
                              Seq(Assign("result", Mul(Var "result", Var "x")),
                                  Assign("x", Var "x" .-. Num 1)))))))
    
    let factorial2 x =
        Seq(Declare "result",
            Seq(Declare "x",
                Seq(Assign("result", Num 1),
                    Seq(Assign("x", Num x),
                        IT (Lt(Num 0, Var "x"),
                        While(Lt(Num 0, Var "x"),
                              Seq(Assign("result", Mul(Var "result", Var "x")),
                                  Assign("x", Var "x" .-. Num 1))))))))
                            

    let factorial_err1 x =
        Seq(Declare "result",
            Seq(Declare "result",
                Seq(Assign("result", Num 1),
                    Seq(Assign("x", Num x),
                        While(Lt(Num 0, Var "x"),
                              Seq(Assign("result", Mul(Var "result", Var "x")),
                                  Assign("x", Var "x" .-. Num 1)))))))
        
    let factorial_err2 x =
        Seq(Declare "result",
            Seq(Declare "x",
                Seq(Assign("result", Num 1),
                    Seq(Assign("x", Num x),
                        While(Lt(Num 0, Var "x"),
                              Seq(Assign("result", Mul(Var "result", Var "y")),
                                  Assign("x", Var "x" .-. Num 1)))))))
    
    let (.++.) a b = Seq(a, b)

        
    let factorial_err3 x =
        Seq(Declare "result",
            Seq(Declare "x",
                Seq(Assign("result", Num 1),
                    Seq(Assign("x", Num x),
                        While(Lt(Num 0, Var "x"),
                              Seq(Assign("result", Div(Var "result", Var "x" .-. Var "x")),
                                  Assign("x", Var "x" .-. Num 1)))))))

    let factorialMem x =
        Seq(Declare "ptr",
            Seq(Alloc("ptr", Num x),
                Seq(Declare "x",
                    Seq(Assign("x", Num x),
                        Seq(While(Lt(Num 0, Var "x"),
                                  Seq(MemWrite(Var "ptr" .+. (Var "x" .-. Num 1), Var "x"),
                                      Assign("x", Var "x" .-. Num 1))),
                            Seq(Assign("x", Num (x - 1)),
                                Seq(Declare "result",
                                    Seq(Assign("result", Num 1),
                                        Seq(While(Num 0 .<=. Var "x",
                                                  Seq(Assign("result", Mul(Var "result", MemRead(Var "ptr" .+. Var "x"))),
                                                      Assign("x", Var "x" .-. Num 1))),
                                            Free(Var "ptr", Num x))))))))))
    let factorialMem_err1 x =
        Seq(Declare "ptr",
            Seq(Alloc("ptr", Num (x - 1)),
                Seq(Declare "x",
                    Seq(Assign("x", Num x),
                        Seq(While(Lt(Num 0, Var "x"),
                                  Seq(MemWrite(Var "ptr" .+. (Var "x" .-. Num 1), Var "x"),
                                      Assign("x", Var "x" .-. Num 1))),
                            Seq(Assign("x", Num (x - 1)),
                                Seq(Declare "result",
                                    Seq(Assign("result", Num 1),
                                        Seq(While(Num 0 .<=. Var "x",
                                                  Seq(Assign("result", Mul(Var "result", MemRead(Var "ptr" .+. Var "x"))),
                                                      Assign("x", Var "x" .-. Num 1))),
                                            Free(Var "ptr", Num x))))))))))

    let factorialMem_err2 x =
        Seq(Declare "ptr",
            Seq(Alloc("ptr", Num x),
                Seq(Declare "x",
                    Seq(Assign("x", Num x),
                        Seq(While(Lt(Num 0, Var "x"),
                                  Seq(MemWrite(Var "ptr" .+. (Var "x" .-. Num 1), Var "x"),
                                      Assign("x", Var "x" .-. Num 1))),
                            Seq(Assign("x", Num (x - 1)),
                                Seq(Declare "result",
                                    Seq(Assign("result", Num 1),
                                        Seq(While(Num 0 .<=. Var "x",
                                                  Seq(Assign("result", Mul(Var "result", MemRead(Var "ptr" .+. Var "x"))),
                                                      Assign("x", Var "x" .-. Num 1))),
                                            Free(Var "ptr", Num (x + 1)))))))))))

    let factorialMem_err3 x =
        Seq(Declare "ptr",
            Seq(Alloc("ptr", Num x),
                Seq(Declare "x",
                    Seq(Assign("x", Num x),
                        Seq(While(Lt(Num 0, Var "x"),
                                  Seq(MemWrite(Var "ptr" .+. Var "x", Var "x"),
                                      Assign("x", Var "x" .-. Num 1))),
                            Seq(Assign("x", Num (x - 1)),
                                Seq(Declare "result",
                                    Seq(Assign("result", Num 1),
                                        Seq(While(Num 0 .<=. Var "x",
                                                  Seq(Assign("result", Mul(Var "result", MemRead(Var "ptr" .+. Var "x"))),
                                                      Assign("x", Var "x" .-. Num 1))),
                                            Free(Var "ptr", Num x))))))))))

    let factorialMem_err4 x =
        Seq(Declare "ptr",
            Seq(Alloc("ptr", Num x),
                Seq(Declare "x",
                    Seq(Assign("x", Num x),
                        Seq(While(Lt(Num 0, Var "x"),
                                  Seq(MemWrite(Var "ptr" .+. (Var "x" .-. Num 1), Var "x"),
                                      Assign("x", Var "x" .-. Num 1))),
                            Seq(Assign("x", Num (x - 1)),
                                Seq(Declare "result",
                                    Seq(Assign("result", Num 1),
                                        Seq(While(Num 0 .<=. Var "x",
                                                  Seq(Assign("result", Mul(Var "result", MemRead(Var "ptr" .+. Var "x" .+. Num 1))),
                                                      Assign("x", Var "x" .-. Num 1))),
                                            Free(Var "ptr", Num x))))))))))
                                            
    let factorialMem_err5 x =
        Seq(Declare "ptr",
            Seq(Alloc("ptr", Num -x),
                Seq(Declare "x",
                    Seq(Assign("x", Num x),
                        Seq(While(Lt(Num 0, Var "x"),
                                  Seq(MemWrite(Var "ptr" .+. (Var "x" .-. Num 1), Var "x"),
                                      Assign("x", Var "x" .-. Num 1))),
                            Seq(Assign("x", Num (x - 1)),
                                Seq(Declare "result",
                                    Seq(Assign("result", Num 1),
                                        Seq(While(Num 0 .<=. Var "x",
                                                  Seq(Assign("result", Mul(Var "result", MemRead(Var "ptr" .+. Var "x" .+. Num 1))),
                                                      Assign("x", Var "x" .-. Num 1))),
                                            Free(Var "ptr", Num x))))))))))

    let factorialMem_err6 x =
        Declare "ptr" />
        Alloc("ptr", Num x) />
        Declare "x" />
        ("x" .<-. Num x) />
        While(Num 0 .<. Var "y",
              MemWrite(Var "ptr" .+. (Var "x" .-. Num 1), Var "x") />
              ("x" .<-. Var "x" .-. Num 1)) />
        ("x" .<-. Num (x - 1)) />
        Declare "result" />
        ("result" .<-. Num 1) />
        While(Num 0 .<=. Var "x",
              ("result" .<-. Var "result" .*. MemRead(Var "ptr" .+. Var "x")) />
              ("x"      .<-. Var "x" .-. Num 1)) />
        Free(Var "ptr", Num x)

    let guessANumber =
          Declare "maxNum" />
          Print([], "Input a maximum number: ") />
          ("maxNum" .<-. Read) />
          Declare "answer" />
          ("answer" .<-. (Random .%. Var "maxNum") .+. Num 1) />
          Declare "guess" />
          ("guess" .<-. Num -1) />
          While(Var "guess" .<>. Var "answer",
                Print([Var "maxNum"], "Input a number between 1 and %: ") />
                ("guess" .<-. Read) />
                If(Var "guess" .<. Var "answer",
                   Print([Var "guess"], "You guessed %, which is too low, please try again\n"),
                   If(Var "guess" .>. Var "answer",
                      Print([Var "guess"], "You guessed %, which is too high, please try again\n"),
                      Print([Var "answer"], "Congratulations, % was the correct number"))))
                       
    let bubbleSort =
        Declare "max" />
        Print([], "Input a maximum number in the array: ") />
        ("max" .<-. Read) />
        Declare "size" />
        Print([], "Input the size of the array: ") />
        ("size" .<-. Read) />
        Declare "ptr" />
        Alloc("ptr", Var "size") />
        For("x",
            Var "ptr",
            Var "x" .<. Var "ptr" .+. Var "size",
            "x" .<-. Var "x" .+. Num 1,
            MemWrite(Var "x", Random .%. (Var "max" .+. Num 1))) />
        Print([], "Original array: ") />
        For("y",
            Var "ptr",
            Var "y" .<. Var "ptr" .+. Var "size",
            "y" .<-. Var "y" .+. Num 1,
            Print([MemRead (Var "y")], "% ")) />
        Print([], "\n") />
        Declare "swapped" />
        Declare "i" />
        Declare "temp" />
        ("swapped" .<-. Num 1) />
        While(Var "swapped" .<>. Num 0,
              ("swapped" .<-. Num 0) />
              ("i" .<-. Var "ptr" .+. Num 1) />
              While(Var "i" .<. Var "ptr" .+. Var "size",
                    IT(MemRead(Var "i") .<. MemRead(Var "i" .-. Num 1),
                       ("temp" .<-. MemRead(Var "i")) />
                       MemWrite(Var "i", MemRead(Var "i" .-. Num 1)) />
                       MemWrite(Var "i" .-. Num 1, Var "temp") />
                       ("swapped" .<-. Num 1)) />
                    ("i" .<-. Var "i" .+. Num 1))) />
        Print([], "Sorted array: ") />
        For("z",
            Var "ptr",
            Var "z" .<. Var "ptr" .+. Var "size",
            "z" .<-. Var "z" .+. Num 1,
            Print([MemRead (Var "z")], "% "))

    let recFactorialFun =
        If(Var "x" .=. Num 0, Return (Num 1), Return (FunctionCall("factorial", [Var "x" .-. Num 1]) .*. Var "x" ) /> Print ([Var "x"], "This should never happen %."))
        
    let factorialProg : program = Map.ofList(["factorial", (["x"], recFactorialFun)])
    let recFactorial x =
        Declare "result" />
        ("result" .<-. FunctionCall("factorial", [Num x]))
        
                    
        
      
