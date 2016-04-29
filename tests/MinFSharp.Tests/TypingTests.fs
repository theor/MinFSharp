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

    let d ast (str:Syntax.t<Unit>) =
        TestCaseData(str, ast).SetName(sprintf "%A" str)
    type Tcs() =
        static member Data() =
            [| d Type.Int (Int 42)
               d Type.Int (Let((Id("x"),Type.Unit), (Int 3), Int 42))
               d Type.Int (Let((Id("x"),Type.Unit), (Int 3), Var(Id("x"))))
            |]// |> Array.map d
    [<Test>]
    [<TestCaseSource(typeof<Tcs>, "Data")>]
    let ``sdfsdf`` (ast:Syntax.t<Unit>) t =
        Typing.typing Map.empty ast |> shouldEqual (ok t)

