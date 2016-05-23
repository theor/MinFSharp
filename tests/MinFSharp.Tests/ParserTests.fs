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
    open FSharp.Object.Diff

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
                d "1 <!> 2" (BinOp("<!>", Int 1 @@ (1L,1L), Int 2 @@ (1L,7L)))
                d "1<!>2" (BinOp("<!>", Int 1 @@ (1L,1L), Int 2 @@ (1L,5L)))
                d "1 >> 2" (BinOp(">>", Int 1 @@ (1L,1L), Int 2 @@ (1L,6L)))
                d "1 !@~<> 2" (BinOp("!@~<>", Int 1 @@ (1L,1L), Int 2 @@ (1L,9L)))
                d "f 42 13" (appId "f" [Int 42; Int 13])
                d "(f 42 13)" (appId "f" [Int 42; Int 13])
                d "(f (g 42) 13)" (App(Var(Id "f"), [appId "g" [Int 42]; Int 13]))
                d "let x = 7 in\nx" (LetIn(Syntax.Decl((Id "x"), Type.genType), Int 7,
                                           Some <| Var(Id "x")))
                d "let x = 1 in\nlet y = 2 in\n x+y" (LetIn(Syntax.Decl((Id "x"), Type.genType), Int 1,
                                                            (LetIn(Syntax.Decl((Id "y"), Type.genType), Int 2,
                                                                   BinOp("+", Var(Id "x") @@ (3L,2L), Var(Id "y") @@ (3L,4L))|>Some))|>Some))
                d "1;2" (Seq [Int 1 @@ (1L,1L); Int 2 @@ (1L,3L)])
                d "1\n2" (Seq [Int 1 @@ (1L,1L); Int 2 @@ (2L,1L)])
                d "let x : int = 7 in\nx" (LetIn(Syntax.Decl((Id "x"), Type.Int), Int 7,
                                                 Some <| Var(Id "x")))
                d "let x : bool = 7 in\nx" (LetIn(Syntax.Decl((Id "x"), Type.Bool), Int 7,
                                                 Some <| Var(Id "x")))
                d "let x : int array = () in ()" (LetIn(Syntax.Decl(Id "x", Type.Array Type.Int), Unit, Some Unit))
                d "let x : int * bool = () in ()" (LetIn(Syntax.Decl(Id "x", Type.Tuple [Type.Int; Type.Bool]), Unit, Some Unit))
                d "let x : int -> bool -> float = () in ()" (LetIn(Syntax.Decl(Id "x", Type.arrow [Type.Int; Type.Bool; Type.Float]), Unit, Some Unit))
                d "true" (Bool true)
                d "false" (Bool false)
                d "if true then 1 else 2" (If(Bool true @@ (1L, 4L), Int 1 @@ (1L,14L), Int 2 @@ (1L,21L)))
                d "if true then\n  1\nelse\n  2" (If(Bool true @@ (1L,4L), Int 1 @@ (2L,3L), Int 2 @@ (4L,3L)))
                d "if (f 42) then 1 else 2" (If((appId "f" [Int 42]) @@ (1L,4L), Int 1 @@ (1L,16L), Int 2 @@ (1L,23L)))
                d "let min x y = if x < y then x else y" (LetIn(Syntax.Decl(Id "min", Type.genType),
                                                                (FunDef([Decl(Id "x", Type.genType);Decl(Id "y", Type.genType)],
                                                                        FBody.Body ((If (BinOp ("<",Var (Id "x") @@ (1L,18L), Var (Id "y")@@ (1L,22L)) @@ (1L,18L),
                                                                                         Var (Id "x") @@ (1L,29L),
                                                                                         Var (Id "y") @@ (1L,36L)))), Type.genType)),
                                                                None))
                d "let f x y = y" (LetIn(Syntax.Decl(Id "f", Type.genType), (FunDef([Decl(Id "x", Type.genType);Decl(Id "y", Type.genType)],
                                                                                     FBody.Body << Var <| Id "y", Type.genType)),
                                         None))
                d "let fact n =\
                     if n <= 1 then 1
                     else n * (fact (n - 1))"
                   (LetIn(Syntax.Decl(Id "fact", Type.genType),
                          FunDef([Decl(Id "n", Type.genType)],
                                 Body(If(BinOp ("<=",Var (Id "n") @@ (1L,16L),Int 1 @@ (1L,21L)) @@ (1L,16L),
                                         Int 1 @@ (1L,28L),
                                         BinOp("*",Var (Id "n") @@ (2L,27L),
                                                   App (Var (Id "fact"),
                                                        [BinOp ("-", Var (Id "n") @@ (2L,38L), Int 1 @@ (2L,42L))]) @@ (2L,31L)) @@ (2L,27L))),
                                 Type.genType),None))
                d "let fact n =\
                     if n <= 1 then 1
                     else n * (fact (n-1))"
                   (LetIn(Syntax.Decl(Id "fact", Type.genType),
                          FunDef([Decl(Id "n", Type.genType)],
                                 Body(If(BinOp ("<=",Var (Id "n") @@ (1L,16L),Int 1 @@ (1L,21L)) @@ (1L,16L),
                                         Int 1 @@ (1L,28L),
                                         BinOp("*",Var (Id "n") @@ (2L,27L),
                                                   App (Var (Id "fact"),
                                                        [BinOp ("-", Var (Id "n") @@ (2L,38L), Int 1 @@ (2L,40L))]) @@ (2L,31L)) @@ (2L,27L))),
                                 Type.genType),None))
            |]

    type DiffVisitor() =
        interface NodeVisitor with
            member x.Node(arg1: DiffNode, arg2: NodeVisit): unit =
                if arg1.State = DiffNodeState.Changed then
                    printfn "%A-%A\n" arg1.Type.Name arg1.Path


    let diff a b =
//        let d = DiffMatchPatch.diff_match_patch()
//        let ta = sprintf "%A" a
//        let tb = sprintf "%A" b
//        let diffs = d.patch_make(ta, tb)
//        let txt = d.patch_toText diffs
//        printfn "\n\ndiff:\n%s" txt

        let delta = ObjectDifferBuilder.BuildDefault().Compare(a, b)
        delta.Visit(PrintingVisitor(a,b))
        delta.Visit(DiffVisitor())

    let testParseOk (s:string) (a:Syntax.t) =
        match MinFSharp.Parser.parseU (ignore (*printf "%A"*)) s with
        | Ok(ast,_) ->
            printf "%A" ast
            diff ast a
            ast |> shouldEqual a
            let env = ref Env.newTypeEnv
            trial {
                let! typed = Typing.typed env ast
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
        let pz a = a @= Pos.zero
        let a = FunDef([Decl(Id "n", Type.genType)],
                                 Body(If(BinOp ("<=",Var (Id "n") @= Pos.zero, Int 1 @= Pos.zero) |> pz,
                                         Int 1 |> pz,
                                         BinOp("*",Var (Id "n") @= Pos.zero,
                                                   App (Var (Id "fact"),
                                                        [BinOp ("-", Var (Id "n") @= Pos.zero,Int -1 @= Pos.zero)]) @= Pos.zero) |> pz)), Type.genType)
        a |> shouldEqual a