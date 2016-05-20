namespace MinFSharp.Tests

module TypingTests =

    open MinFSharp
    open MinFSharp.Syntax
    open MinFSharp.Interpreter
    open MinFSharp.Identifier
//    open MinFSharp.Type
    open NUnit.Framework
    open FsUnitTyped
    open Chessie.ErrorHandling

    let d ty (ast:Syntax.t<Unit>) =
        TestCaseData(ast, ty, true).SetName(sprintf "%A" ast)
    let f ty (ast:Syntax.t<Unit>) =
        TestCaseData(ast, ty, false).SetName(sprintf "%A" ast)
    type Tcs() =
        static member Data() =
            [| d Type.Int (Int 42)
               d (Type.Fun([Type.Int; Type.Int], Type.Int)) (Var (Identifier.Id "(+)"))
               d Type.Int (BinOp("+", Int 42, Int 42))
               f Type.Int (BinOp("+", Int 42, Float 42.0))
               d Type.Int (LetIn((Id("x"),Type.Unit), (Int 3), Some <| Int 42))
               d Type.Int (LetIn((Id("x"),Type.Unit), (Int 3), Some << Var <| Id "x"))
            |]// |> Array.map d
    [<Test>]
    [<TestCaseSource(typeof<Tcs>, "Data")>]
    let ``sdfsdf`` (ast:Syntax.t<Unit>) t passes =
        match passes, Typing.typed Env.newEnv ast with
        | true, Fail(_) -> failwith "should pass"
        | false, Fail(_) -> ()
        | false, Pass(ast, ty) -> failwith "should fail"
        | true, Pass(ast, ty) ->
            printf "%O\n" ty
            printf "%O\n" ast
            ty |> shouldEqual t

