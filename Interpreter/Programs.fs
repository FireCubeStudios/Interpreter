module Interpreter.Programs

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
        
    let factorial_err3 x =
        Seq(Declare "result",
            Seq(Declare "x",
                Seq(Assign("result", Num 1),
                    Seq(Assign("x", Num x),
                        While(Lt(Num 0, Var "x"),
                              Seq(Assign("result", Div(Var "result", Var "x" .-. Var "x")),
                                  Assign("x", Var "x" .-. Num 1)))))))
