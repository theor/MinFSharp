namespace MinFSharp.Tests

[<AutoOpen>]
module Utils =
    open MinFSharp
    open MinFSharp.Syntax
    let sInt i = Lit(Int i)
    let sUnit = Lit(Unit)
    let sBool b = Lit(Bool b)
    let sFloat f = Lit(Float f)

    let pz p = p @= Pos.zero

    let binOp op l r = BinOp(op, pz l, pz r)
    let sif cond ethen eelse = If(pz cond, pz ethen, pz eelse)

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
            [|  d "42" (sInt 42)
                d "(42)" (sInt 42)
                d "x" (Var <| Id "x")
                d "x-1" (BinOp("-", varId "x" @@ (1,1), sInt 1 @@(1,3)))
//                d "=" (Var <| Id "=")
                d "1<2" (BinOp("<", sInt 1 @@ (1,1), sInt 2 @@ (1,3)))
                d "1 <!> 2" (BinOp("<!>", sInt 1 @@ (1,1), sInt 2 @@ (1,7)))
                d "1<!>2" (BinOp("<!>", sInt 1 @@ (1,1), sInt 2 @@ (1,5)))
                d "1 >> 2" (BinOp(">>", sInt 1 @@ (1,1), sInt 2 @@ (1,6)))
                d "1 !@~<> 2" (BinOp("!@~<>", sInt 1 @@ (1,1), sInt 2 @@ (1,9)))
                d "f 42 13" (appId "f" [pz <| sInt 42; pz <| sInt 13])
                d "(f 42 13)" (appId "f" [pz <| sInt 42; pz <| sInt 13])
                d "(f (g 42) 13)" (App(pz <| Var(Id "f"), [pz <| appId "g" [pz <| sInt 42]; pz <| sInt 13]))
                d "let x = 7 in\nx" (LetIn(Syntax.Decl((Id "x"), Type.genType()), pz <| sInt 7,
                                           Some <| pz(Var(Id "x"))))
                d "let x = 1 in\nlet y = 2 in\n x+y" (LetIn(Syntax.Decl((Id "x"), Type.genType()), pz <| sInt 1,
                                                            pz (LetIn(Syntax.Decl((Id "y"), Type.genType()), pz <| sInt 2,
                                                                   pz <| BinOp("+", Var(Id "x") @@ (3,2), Var(Id "y") @@ (3,4))|>Some))|>Some))
                d "1;2" (Seq [sInt 1 @@ (1,1); sInt 2 @@ (1,3)])
                d "1\n2" (Seq [sInt 1 @@ (1,1); sInt 2 @@ (2,1)])
                d "let x : int = 7 in\nx" (LetIn(Syntax.Decl((Id "x"), Type.Int), pz <| sInt 7,
                                                 Some <| pz(Var(Id "x"))))
                d "let x : bool = 7 in\nx" (LetIn(Syntax.Decl((Id "x"), Type.Bool), pz <| sInt 7,
                                                 Some <| pz(Var(Id "x"))))
                d "let x : int array = () in ()" (LetIn(Syntax.Decl(Id "x", Type.Array Type.Int), pz sUnit, Some(pz sUnit)))
                d "let x : int * bool = () in ()" (LetIn(Syntax.Decl(Id "x", Type.Tuple [Type.Int; Type.Bool]), pz sUnit, Some(pz sUnit)))
                d "let x : int -> bool -> float = () in ()" (LetIn(Syntax.Decl(Id "x", Type.arrow [Type.Int; Type.Bool; Type.Float]), pz sUnit, Some(pz sUnit)))
                d "true" (sBool true)
                d "false" (sBool false)
                d "if true then 1 else 2" (If(sBool true @@ (1, 4), sInt 1 @@ (1,14), sInt 2 @@ (1,21)))
                d "if true then\n  1\nelse\n  2" (If(sBool true @@ (1,4), sInt 1 @@ (2,3), sInt 2 @@ (4,3)))
                d "if (f 42) then 1 else 2" (If((appId "f" [pz <| sInt 42]) @@ (1,4), sInt 1 @@ (1,16), sInt 2 @@ (1,23)))
                d "let min x y = if x < y then x else y" (LetIn(Syntax.Decl(Id "min", Type.genType()),
                                                                (Pos.zero, FunDef([Decl(Id "x", Type.genType());Decl(Id "y", Type.genType())],
                                                                        FBody.Body (pz (If (BinOp ("<",Var (Id "x") @@ (1,18), Var (Id "y")@@ (1,22)) @@ (1,18),
                                                                                         Var (Id "x") @@ (1,29),
                                                                                         Var (Id "y") @@ (1,36)))), Type.genType())),
                                                                None))
                d "let f (x:int) : int = x" (LetIn(Syntax.Decl(Id "f", Type.genType()), (Pos.zero, FunDef([Decl(Id "x", Type.Int)],
                                                                                                FBody.Body(pz <|varId "x"), Type.Int)),
                                                                                                None))
                d "let f x y = y" (LetIn(Syntax.Decl(Id "f", Type.genType()), (Pos.zero, FunDef([Decl(Id "x", Type.genType());Decl(Id "y", Type.genType())],
                                                                                      FBody.Body(pz <|varId "y"), Type.genType())),
                                                                                      None))
                d "let f (x) = x" (LetIn(Syntax.Decl(Id "f", Type.genType()), (Pos.zero, FunDef([Decl(Id "x", Type.genType())],
                                                                                      FBody.Body(pz <|varId "x"), Type.genType())),
                                                                                      None))
                d "let fact n =\
                     if n <= 1 then 1
                     else n * (fact (n - 1))"
                   (LetIn(Syntax.Decl(Id "fact", Type.genType()),
                          (Pos.zero, FunDef([Decl(Id "n", Type.genType())],
                                 Body(pz <| If(BinOp ("<=",Var (Id "n") @@ (1,16),sInt 1 @@ (1,21)) @@ (1,16),
                                         sInt 1 @@ (1,28),
                                         BinOp("*",Var (Id "n") @@ (2,27),
                                                   App (pz <| Var (Id "fact"),
                                                        [pz <| BinOp ("-", Var (Id "n") @@ (2,38), sInt 1 @@ (2,42))]) @@ (2,31)) @@ (2,27))),
                                 Type.genType())),None))
                d "let fact n =\
                     if n <= 1 then 1
                     else n * (fact (n-1))"
                   (LetIn(Syntax.Decl(Id "fact", Type.genType()),
                          (Pos.zero, FunDef([Decl(Id "n", Type.genType())],
                                 Body(pz <| If(BinOp ("<=",Var (Id "n") @@ (1,16),sInt 1 @@ (1,21)) @@ (1,16),
                                         sInt 1 @@ (1,28),
                                         BinOp("*",Var (Id "n") @@ (2,27),
                                                   App (pz <| Var (Id "fact"),
                                                        [pz <| BinOp ("-", Var (Id "n") @@ (2,38), sInt 1 @@ (2,40))]) @@ (2,31)) @@ (2,27))),
                                 Type.genType())),None))
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
        | Ok((pos,ast),_) ->
            printf "%A" pos
            printf "%A" ast
            try printf "\nPrettyPrint:\n%s\n" (PrettyPrinter.print PrettyPrinter.defaultOptions ast) with e -> printfn "%O" e
            diff ast a
            ast |> shouldEqual a
            let env = ref (Env.newTypeEnv())
            trial {
                let! typed = Typing.typed env ast
                do printfn "\n\nTYPED:\n%A" typed
            }
        | Bad(e) ->  failwith (e.ToString())

    [<Test>]
    [<TestCaseSource(typeof<TCS>, "Data")>]
    let ``parsing tests`` (s:string,a:Syntax.t) =
        FParsecTrace.print <- false
        ignore <| testParseOk s a

    [<Test>]
    let ``fbody equality test`` () =
        let pz a = a @= Pos.zero
        let a = FunDef([Decl(Id "n", Type.genType())],
                                 Body(pz <| If(BinOp ("<=",Var (Id "n") @= Pos.zero, sInt 1 @= Pos.zero) |> pz,
                                         sInt 1 |> pz,
                                         BinOp("*",Var (Id "n") @= Pos.zero,
                                                   App (pz <| Var (Id "fact"),
                                                        [pz <| BinOp ("-", Var (Id "n") @= Pos.zero,sInt -1 @= Pos.zero)]) @= Pos.zero) |> pz)), Type.genType())
        a |> shouldEqual a