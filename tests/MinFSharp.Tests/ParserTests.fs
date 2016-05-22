namespace MinFSharp.Tests

module ParserTests =

    open System
    open MinFSharp
    open MinFSharp.Syntax
    open MinFSharp.Interpreter
    open MinFSharp.Identifier
    open NUnit.Framework
    open FsUnitTyped
    open Chessie.ErrorHandling

    let dn name str ast  =
        TestCaseData(str, ast).SetName(name)
    let d str (ast:Syntax.t)  =
        TestCaseData(str, ast).SetName(str)
    let i (tcs:TestCaseData) = tcs.Ignore()
    type TCS() =
        static member Data() =
            [|  d "42" (Int 42)
                d "(42)" (Int 42)
                d "x" (Var <| Id "x")
                d "x-1" (BinOp("-", varId "x" @@ (1L,1L), Int 1 @@(1L,3L)))
//                d "=" (Var <| Id "=")
                d "1<2" (BinOp("<", Int 1 @@ (1L,1L), Int 2 @@ (1L,3L)))
                d "1 <!> 2" (BinOp("<!>", Int 1 @@ (1L,1L), Int 2 @@ (1L,6L)))
                d "1<!>2" (BinOp("<!>", Int 1 @@ (1L,1L), Int 2 @@ (1L,3L)))
                d "1 >> 2" (BinOp(">>", Int 1 @@ (1L,1L), Int 2 @@ (1L,5L)))
                d "1 !@~<> 2" (BinOp("!@~<>", Int 1 @@ (1L,1L), Int 2 @@ (1L,9L)))
                d "f 42 13" (appId "f" [Int 42; Int 13])
                d "(f 42 13)" (appId "f" [Int 42; Int 13])
                d "(f (g 42) 13)" (App(Var(Id "f"), [appId "g" [Int 42]; Int 13]))
                d "let x = 7 in\nx" (LetIn(((Id "x"), Type.Var None), Int 7,
                                           Some <| Var(Id "x")))
                d "let x = 1 in\nlet y = 2 in\n x+y" (LetIn(((Id "x"), Type.Var None), Int 1,
                                                            (LetIn(((Id "y"), Type.Var None), Int 2,
                                                                   BinOp("+", Var(Id "x") @@ (3L,2L), Var(Id "y") @@ (3L,4L))|>Some))|>Some))
                d "1;2" (Seq [Int 1; Int 2])
                d "1\n2" (Seq [Int 1; Int 2])
                d "let x : int = 7 in\nx" (LetIn(((Id "x"), Type.Int), Int 7,
                                                 Some <| Var(Id "x")))
                d "let x : bool = 7 in\nx" (LetIn(((Id "x"), Type.Bool), Int 7,
                                                 Some <| Var(Id "x")))
                d "let x : int array = () in ()" (LetIn((Id "x", Type.Array Type.Int), Unit, Some Unit))
                d "let x : int * bool = () in ()" (LetIn((Id "x", Type.Tuple [Type.Int; Type.Bool]), Unit, Some Unit))
                d "let x : int -> bool -> float = () in ()" (LetIn((Id "x", Type.arrow [Type.Int; Type.Bool; Type.Float]), Unit, Some Unit))
                d "true" (Bool true)
                d "false" (Bool false)
                d "if true then 1 else 2" (If(Bool true, Int 1, Int 2))
                d "if true then\n  1\nelse\n  2" (If(Bool true, Int 1, Int 2))
                d "if (f 42) then 1 else 2" (If((appId "f" [Int 42]), Int 1, Int 2))
                d "let min x y = if x < y then x else y" (LetIn((Id "min", Type.Var None),
                                                                (FunDef([(Id "x", Type.Var None);(Id "y", Type.Var None)],
                                                                        FBody.Body ((If (BinOp ("<",Var (Id "x") @= Pos.zero, Var (Id "y") @= Pos.zero),
                                                                                         Var (Id "x"),
                                                                                         Var (Id "y")))), Type.Var None)),
                                                                None))
                d "let f x y = y" (LetIn((Id "f", Type.Var None), (FunDef([(Id "x", Type.Var None);(Id "y", Type.Var None)],
                                                                          FBody.Body << Var <| Id "y", Type.Var None)),
                                         None))
                d "let fact n =\
                     if n <= 1 then 1
                     else n * (fact (n - 1))"
                   (LetIn((Id "fact", Type.Var None),
                          FunDef([(Id "n", Type.Var None)],
                                 Body(If(BinOp ("<=",Var (Id "n") @= Pos.zero,Int 1 @= Pos.zero),
                                         Int 1,
                                         BinOp("*",Var (Id "n") @= Pos.zero,
                                                   App (Var (Id "fact"),
                                                        [BinOp ("-", Var (Id "n") @= Pos.zero, Int 1 @= Pos.zero)]) @= Pos.zero))),
                                 Type.Var None),None))
                d "let fact n =\
                     if n <= 1 then 1
                     else n * (fact (n-1))"
                   (LetIn((Id "fact", Type.Var None),
                          FunDef([(Id "n", Type.Var None)],
                                 Body(If(BinOp ("<=",Var (Id "n") @= Pos.zero,Int 1 @= Pos.zero),
                                         Int 1,
                                         BinOp("*",Var (Id "n") @= Pos.zero,
                                                   App (Var (Id "fact"),[BinOp ("-", Var (Id "n") @= Pos.zero,Int 1 @= Pos.zero)]) @= Pos.zero))),
                                 Type.Var None),None))
            |]

    let testParseOk (s:string) (a:Syntax.t) =
        match MinFSharp.Parser.parseU (ignore (*printf "%A"*)) s with
        | Ok(ast,_) ->
            printf "%A" ast
            ast |> shouldEqual a
            trial {
                let! typed = Typing.typed Env.newEnv ast
                do printfn "\n\nTYPED:\n%A" typed
            }
        | Bad(e) ->  failwith (e.ToString())

    [<Test>]
    [<TestCaseSource(typeof<TCS>, "Data")>]
    let ``parsing tests`` (s:string,a:Syntax.t) =
        //FParsecTrace.print <- true
        ignore <| testParseOk s a

    [<Test>]
    let ``fbody equality test`` () =
        let a = FunDef([(Id "n", Type.Var None)],
                                 Body(If(BinOp ("<=",Var (Id "n") @= Pos.zero, Int 1 @= Pos.zero),
                                         Int 1,
                                         BinOp("*",Var (Id "n") @= Pos.zero,
                                                   App (Var (Id "fact"),[BinOp ("-", Var (Id "n") @= Pos.zero,Int -1 @= Pos.zero)]) @= Pos.zero))), Type.Var None)
        a |> shouldEqual a