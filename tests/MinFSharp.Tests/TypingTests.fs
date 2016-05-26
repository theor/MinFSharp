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

    let d ty (ast:Syntax.t) =
        TestCaseData(ast, ty, true).SetName(sprintf "%A" ast)
    let f ty (ast:Syntax.t) =
        TestCaseData(ast, ty, false).SetName(sprintf "%A" ast)

    let pz p = p @= Pos.zero
    type Tcs() =
        static member Data() =
            [| d Type.Int (Int 42)
               d (Type.Fun(Type.poly 0u, Type.poly 0u)) (Syntax.varId "id")

               d (Type.arrow[Type.Int; Type.Int; Type.Int]) (Var (Identifier.Id "(+)"))
               d (Type.arrow[Type.Int; Type.Int; Type.Int]) (Var (Identifier.Id "add"))
               d (Type.Int) (App(Var (Identifier.Id "add"), [Int 1; Int 2]))
               f (Type.Int) (App(Var (Identifier.Id "add"), [Int 1; Float 2.0]))
               f (Type.Int) (App(Var (Identifier.Id "add"), [Int 1; Int 2; Int 3]))
               d (Type.arrow[Type.Int; Type.Int]) (App (Var (Identifier.Id "add"), [Int 1]))

               d (Type.Int) (App(Syntax.varId "id", [Int 1]))

               d (Type.Fun(Type.poly 0u, Type.poly 0u))
                 (Syntax.FunDef([Syntax.Decl(Identifier.Id "x", Type.genType())],
                                Syntax.FBody.Body(Syntax.varId "x"),
                                Type.genType()))
               d (Type.Fun(Type.poly 0u, Type.poly 0u))
                 (Syntax.FunDef([Syntax.Decl(Identifier.Id "x", Type.genType())],
                                Syntax.FBody.Body(Syntax.BinOp("+", pz <| Syntax.varId "x", pz <| Syntax.varId "x")),
                                Type.genType()))

               d Type.Int (BinOp("+", pz <| Int 42, pz <| Int 42))
               f Type.Int (BinOp("+", pz <| Int 42, pz <| Float 42.0))

               d Type.Int (LetIn(Syntax.Decl(Id("x"),Type.genType()), (Int 3), Some <| Int 42))
               d Type.Int (LetIn(Syntax.Decl(Id("x"),Type.genType()), (Int 3), Some <| varId "x"))
               d Type.Int (LetIn(Syntax.Decl(Id("x"),Type.genType()), (Int 3),
                                 Some <| varId "x"))
               f Type.Int (LetIn(Syntax.Decl(Id("x"),Type.Unit), (Int 3), Some <| varId "x"))

               d Type.Int (If(Bool true |> pz, Int 3 |> pz, Int 4 |> pz))
               f Type.Int (If(Bool true |> pz, Float 4.0 |> pz, Int 4 |> pz))
               f Type.Int (If(Float 4.0 |> pz, Int 4 |> pz, Int 5 |> pz))
            |]// |> Array.map d
    [<Test>]
    [<TestCaseSource(typeof<Tcs>, "Data")>]
    let ``test typing`` (ast:Syntax.t) t passes =
        let env = ref (Env.newTypeEnv())
        let typing = Typing.typed env ast
        let ast =  Typing.typed_deref ast
        match passes, typing with
        | true, Fail(e) -> printfn "%A" e; printfn "%A" ast; failwith "should pass"
        | false, Fail(e) -> printfn "%A" e
        | false, Pass(ty) -> printfn "res:%A\n" ast; failwith "should fail"
        | true, Pass( ty) ->
            printf "Type:\n%O\n" ty
            printf "Ast:\n%O\n" ast
            ty |> shouldEqual t
        | _, _ -> failwith "WEIRD"

    [<Test>]
    let ``instantiate 'a -> 'b``() =
        let t = Type.Fun(Type.poly 0u, Type.poly 1u)
        let t2 = Typing.instantiate t [Some Type.Int; Some Type.Bool]
        t2 |> shouldEqual (Type.Fun(Type.Int, Type.Bool))

    [<Test>]
    let ``instantiate 'a -> 'b -> 'a``() =
        let t = Type.arrow [Type.poly 0u; Type.poly 1u; Type.poly 0u]
        let t2 = Typing.instantiate t [Some Type.Int; Some Type.Bool]
        t2 |> shouldEqual (Type.arrow [Type.Int; Type.Bool; Type.Int])
        
    [<Test>]
    let ``instantiate poly 'a -> 'b``() =
        let t = Type.Fun(Type.poly 0u, Type.poly 1u)
        let t2 = Typing.instantiate t [Some Type.Int; None]
        t2 |> shouldEqual (Type.Fun(Type.Int, Type.poly 1u))
