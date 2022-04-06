// Example Programs


namespace functional_interpreter

open types

module programs =
    // 777
    let prog1 = Expr(IntConstant(777))

    // 400 + 74
    // returns: 474
    let prog2 = Expr(BinOp(PLUS, IntConstant(400), IntConstant(74)))

    // let x = 474
    // x / 2
    // returns: 237
    let prog3A =
        Expr(
            Let(
                Name("x"), 
                IntConstant(474), 
                Expr(BinOp(
                        DIV, 
                        Variable(Name("x")), 
                        IntConstant(2))))
    )

    // let x = 444
    // x
    // returns: 444
    let prog3B =
        Expr(
            Let(
                Name("x"), 
                IntConstant(444), 
                Expr(Variable(Name("x")))
    ))

    // ((400 + 74) / 3) == 158
    // returns: true
    let prog3C =
        Expr(
                Eq(
                    BinOp(
                          DIV, 
                          BinOp(
                                PLUS, 
                                IntConstant(400), 
                                IntConstant(74)
                          ), 
                          IntConstant(3)
                    ),
                    IntConstant(158)
                ))

    // let bot = 3 in
    // (let bot = 2 in bot)
    // +
    // (if (bot == 0) then 474/0 else (400+74)/bot)
    // returns: 160
    let prog5 =
        Expr(Let(
                    Name("bot"), 
                    IntConstant(3), 
                    Expr(BinOp(
                          PLUS, 
                          Let(
                                Name("bot"), 
                                IntConstant(2), 
                                Expr(Variable(Name("bot")))
                          ), 
                          If(
                                Eq(Variable(Name("bot")), IntConstant(0)),
                                Expr(BinOp(
                                      DIV,
                                      IntConstant(474),
                                      IntConstant(0)
                                )),
                                Expr(BinOp(
                                      DIV,
                                      BinOp(
                                            PLUS,
                                            IntConstant(400),
                                            IntConstant(74)

                                      ),
                                      Variable(Name("bot"))
                                ))
                          )
                    ))
        ))


    //function f(top,bot) :
    // if (bot == 0) then 0 else top/bot
    //let bot = 3 in
    // (let bot = 2 in bot)
    // +
    // (f(400+74,bot) + f(470+4,0))
    // returns 160

    let private p6LeftExpr =
        Let(
              Name("bot"), 
              IntConstant(2), 
              Expr(Variable(Name("bot")))
        )

    let private p6RightExpr =
        BinOp(
            PLUS,
            FunctionCall(
                  Name("f"),
                  [|
                        BinOp(
                              PLUS,
                              IntConstant(400),
                              IntConstant(74)
                        );
                        Variable(Name("bot"))                        
                  |]
            ),
            FunctionCall(
                  Name("f"),
                  [|
                        BinOp(
                              PLUS,
                              IntConstant(470),
                              IntConstant(4)
                        );
                        IntConstant(0)
                  |]
            )
        )

    let prog6 =
        Expr(FunctionDeclaration(
                    Name("f"), 
                    [|Name("top"); Name("bot")|],
                    If(
                          Eq(Variable(Name("bot")), IntConstant(0)),
                          Expr(IntConstant(0)),
                          Expr(BinOp(
                                DIV,
                                Variable(Name("top")),
                                Variable(Name("bot"))
                          ))
                    ), 
                    Let(
                          Name("bot"), 
                          IntConstant(3), 
                          Expr(BinOp(PLUS, 
                                p6LeftExpr, 
                                p6RightExpr
                          ))
              )
        ))

    //
    // let x = 6 in
    //      set x to x+1;
    //      x + x;
    // returns: 14
    let private p7Set_x =
        SetVar(
            Name("x"),
                BinOp(
                    PLUS,
                    Variable(Name("x")),
                    IntConstant(1)
        ))
    let prog7SeqSetVar =
        Expr(
            Let(
                Name("x"),
                IntConstant(6),
                Seq(
                    [p7Set_x;
                    Expr(BinOp(PLUS, Variable(Name("x")), Variable(Name("x"))))
                    ])
        )
        )

    //
    //  let x = 0 in
    //      let y = 1 in
    //          while ( x != 10)
    //          {
    //              y = y * 2;
    //              x++;
    //          }
    //          y;
    // 
    // returns: 1024
    let p8Loop =
        While(
            Neq(Variable(Name("x")), IntConstant(10)),
                Seq(
                    [SetVar(Name("y"), BinOp(TIMES, Variable(Name("y")), IntConstant(2)));
                     IncVar(Name("x"))]
                    )
        )


    let prog8While =
        Expr(Let(
                Name("x"),
                IntConstant(0),
                Expr(Let(
                        Name("y"),
                        IntConstant(1),
                        Seq(
                            [p8Loop;
                                Expr(Variable(Name("y")))]
                        )
                ))
            ))

