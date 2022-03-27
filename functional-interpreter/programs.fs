// Example Programs


namespace functional_interpreter

open interpreter

module programs =
    // 777
    let p1 = IntConstant(777)

    // 400 + 74
    let p2 = BinOp(PLUS, IntConstant(400), IntConstant(74))

    // let x = 474
    // x / 2
    let p3A = Let(
                Name("x"), 
                IntConstant(474), 
                BinOp(
                      DIV, 
                      Variable(Name("x")), 
                      IntConstant(2)))

    // let x = 444
    // x
    let p3B = Let(
                Name("x"), 
                IntConstant(444), 
                Variable(Name("x"))
    )

    // ((400 + 74) / 3) == 158
    let p3 = Eq(
                BinOp(
                      DIV, 
                      BinOp(
                            PLUS, 
                            IntConstant(400), 
                            IntConstant(74)
                      ), 
                      IntConstant(3)
                ),
                IntConstant(158))

    // let bot = 3 in
    // (let bot = 2 in bot)
    // +
    // (if (bot == 0) then 474/0 else (400+74)/bot)
    let p5 = Let(
                Name("bot"), 
                IntConstant(3), 
                BinOp(
                      PLUS, 
                      Let(
                            Name("bot"), 
                            IntConstant(2), 
                            Variable(Name("bot"))
                      ), 
                      If(
                            Eq(Variable(Name("bot")), IntConstant(0)),
                            BinOp(
                                  DIV,
                                  IntConstant(474),
                                  IntConstant(0)
                            ),
                            BinOp(
                                  DIV,
                                  BinOp(
                                        PLUS,
                                        IntConstant(400),
                                        IntConstant(74)

                                  ),
                                  Variable(Name("bot"))
                            )
                      )
                )
    )

    //function f(top,bot) :
    // if (bot == 0) then 0 else top/bot
    //let bot = 3 in
    // (let bot = 2 in bot)
    // +
    // (f(400+74,bot) + f(470+4,0))
    let p6 = FunctionDeclaration(
                Name("f"), 
                [|Name("top"); Name("bot")|],
                If(
                      Eq(Variable(Name("bot")), IntConstant(0)),
                      IntConstant(0),
                      BinOp(
                            DIV,
                            Variable(Name("top")),
                            Variable(Name("bot"))
                      )
                ), 
                Let(
                      Name("bot"), 
                      IntConstant(3), 
                      BinOp(
                            PLUS, 
                            Let(
                                  Name("bot"), 
                                  IntConstant(2), 
                                  Variable(Name("bot"))
                            ), 
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
                      )
          )
    )