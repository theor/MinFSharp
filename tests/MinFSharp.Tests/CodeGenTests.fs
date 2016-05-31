namespace MinFSharp.Tests

module CodeGenTests =

    open MinFSharp
    open MinFSharp.Syntax
    open MinFSharp.Identifier
    open NUnit.Framework
    open Chessie.ErrorHandling
    open FsUnitTyped
    
    let t (ast:Syntax.t) = TestCaseData(ast)
    let print l = Syntax.App (Var(Id "printf"), l)
    type TCS() =
        static member Data() =
            [| t (Lit(Int 42))
               t (Syntax.App (Var(Id "printf"), [Lit(Int 13)]))
               t (Syntax.App (Var(Id "printf"), [binOp "+" (sInt 13) (sInt 2)]))
               t (Syntax.App (Var(Id "printf"), [binOp "+" (binOp "+" (sInt 13) (sInt 2)) (sInt 5)]))
               t (LetIn(Syntax.Decl(Id("x"),Type.genType()), (sInt 3), Some <| (print [varId "x"])))
               t (sif (sBool true) (print [sInt 1]) (print [sInt 2]))
               t (sif (binOp "<" (sInt 2) (sInt 1)) (print [sInt 1]) (print [sInt 2]))
            |]

    [<Test>]
    [<TestCaseSource(typeof<TCS>, "Data")>]
    let ``gen asm`` (ast:Syntax.t) =
        let senv = ref (Env.newSymbolEnv())
        let env = ref (Env.newTypeEnv())
        let r = trial {
            let! t = ast |> Typing.typed env
            let ast = Typing.typed_deref ast
            Codegen.gen ast env senv "test.exe"
        }
        match r with
        | Pass _ -> ()
        | Fail e -> failwithf "ERROR %A" e
        | _ -> failwithf "ERROR %A" r