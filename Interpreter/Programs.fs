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
