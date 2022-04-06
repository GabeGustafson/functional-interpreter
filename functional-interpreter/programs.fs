// Example Programs


namespace functional_interpreter

open types

module programs =
    // 777
    let prog1 = Expr(IntConstant(777))

    // 700 + 77
    // returns: 777
    let prog2 = Expr(BinOp(PLUS, IntConstant(700), IntConstant(77)))

    // let x = 700
    // 700 / 2
    // returns: 350
    let prog3A =
        Expr(
            Let(
                Name("x"), 
                IntConstant(700), 
                Expr(BinOp(
                        DIV, 
                        Variable(Name("x")), 
                        IntConstant(2))))
    )

    // let x = 777
    // x
    // returns: 777
    let prog3B =
        Expr(
            Let(
                Name("x"), 
                IntConstant(777), 
                Expr(Variable(Name("x")))
    ))

    // ((700 + 77) / 3) == 259
    // returns: true
    let prog3C =
        Expr(
                Eq(
                    BinOp(
                          DIV, 
                          BinOp(
                                PLUS, 
                                IntConstant(700), 
                                IntConstant(77)
                          ), 
                          IntConstant(3)
                    ),
                    IntConstant(259)
                ))

    // let denom = 3 in
    // (let denom = 2 in denom)
    // +
    // (if (denom == 0) then 777/0 else (700+77)/denom)
    // 
    // returns: 261
    let prog5 =
        Expr(Let(
                    Name("denom"), 
                    IntConstant(3), 
                    Expr(BinOp(
                          PLUS, 
                          Let(
                                Name("denom"), 
                                IntConstant(2), 
                                Expr(Variable(Name("denom")))
                          ), 
                          If(
                                Eq(Variable(Name("denom")), IntConstant(0)),
                                Expr(BinOp(
                                      DIV,
                                      IntConstant(777),
                                      IntConstant(0)
                                )),
                                Expr(BinOp(
                                      DIV,
                                      BinOp(
                                            PLUS,
                                            IntConstant(700),
                                            IntConstant(77)

                                      ),
                                      Variable(Name("denom"))
                                ))
                          )
                    ))
        ))


    // function f(top,bot) :
    //    if (bot == 0) then 0 else top/bot
    //
    //let bot = 3 in
    // (let bot = 2 in bot)
    // +
    // (f(700+77,bot) + f(770+7,0))
    // 
    // returns: 261

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
                              IntConstant(700),
                              IntConstant(77)
                        );
                        Variable(Name("bot"))                        
                  |]
            ),
            FunctionCall(
                  Name("f"),
                  [|
                        BinOp(
                              PLUS,
                              IntConstant(770),
                              IntConstant(7)
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

