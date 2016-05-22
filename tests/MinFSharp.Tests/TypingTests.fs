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
               d (Type.Fun(Type.var "a", Type.var "a")) (Syntax.varId "id")

               d (Type.arrow[Type.Int; Type.Int; Type.Int]) (Var (Identifier.Id "(+)"))
               d (Type.arrow[Type.Int; Type.Int; Type.Int]) (Var (Identifier.Id "add"))
               d (Type.Int) (App(Var (Identifier.Id "add"), [Int 1; Int 2]))
               f (Type.Int) (App(Var (Identifier.Id "add"), [Int 1; Int 2; Int 3]))
               d (Type.arrow[Type.Int; Type.Int]) (App (Var (Identifier.Id "add"), [Int 1]))

               d Type.Int (BinOp("+", Int 42, Int 42))
               f Type.Int (BinOp("+", Int 42, Float 42.0))

               d Type.Int (LetIn((Id("x"),Type.Unit), (Int 3), Some <| Int 42))
               d Type.Int (LetIn((Id("x"),Type.Unit), (Int 3), Some << Var <| Id "x"))

               d Type.Int (If(Bool true, Int 3, Int 4))
               f Type.Int (If(Bool true, Float 4.0, Int 4))
               f Type.Int (If(Float 4.0, Int 4, Int 5))
            |]// |> Array.map d
    [<Test>]
    [<TestCaseSource(typeof<Tcs>, "Data")>]
    let ``test typing`` (ast:Syntax.t<Unit>) t passes =
        match passes, Typing.typed Env.newEnv ast with
        | true, Fail(e) -> printfn "%A" e; failwith "should pass"
        | false, Fail(e) -> printfn "%A" e
        | false, Pass(ast, ty) -> failwith "should fail"
        | true, Pass(ast, ty) ->
            printf "%O\n" ty
            printf "%O\n" ast
            ty |> shouldEqual t
        | _, _ -> failwith "WEIRD"

