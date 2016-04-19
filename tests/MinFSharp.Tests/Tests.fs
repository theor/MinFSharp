namespace MinFSharp.Tests

module EvalTests =

    open MinFSharp
    open MinFSharp.Syntax
    open MinFSharp.Interpreter
    open MinFSharp.Identifier
    open NUnit.Framework
    open FsUnitTyped
    open Chessie.ErrorHandling

    let testEvalRes env ast expRes =
        let ev = eval env ast
        match ev with
        | Bad(e) -> failwith (e.ToString())
        | Ok(e,_) -> e |> shouldEqual expRes

    [<Test>]
    let ``a`` () =
        let ast = Int 42
        testEvalRes Env.newEnv ast ast
    [<Test>]
    let ``app add`` () =
        let ast = App(
                    Var(Id "add"),
                    [Int 42; Int 3])
        testEvalRes Env.newEnv ast (Int 45)
    [<Test>]
    let ``let var then return`` () =
        let ast =
            Let(
                (Id "x", Type.Int),
                Int 13,
                Var (Id "x")
            )
        testEvalRes Env.newEnv ast (Int 13)

    [<Test>]
    let ``function app`` () =
        let ast =
            App(
                (FunDef([Id "x", Type.Int], Body(Var(Id "x")))),
                [Int 13]
            )
        testEvalRes Env.newEnv ast (Int 13)