namespace MinFSharp.Tests

module CodeGenTests =

    open MinFSharp
    open MinFSharp.Syntax
    open MinFSharp.Identifier
    open NUnit.Framework
    open Chessie.ErrorHandling
    open FsUnitTyped
    
    let print l = Syntax.App (Var(Id "printf"), l)
    type Data = Ast of Syntax.t | Txt of string
    let t (ast:Syntax.t) = TestCaseData(Ast ast)
    let p (str) = TestCaseData(Txt str).SetName(str)

    type TCS() =
        static member Data() =
            [| t (Lit(Int 42))
               t (Syntax.App (Var(Id "printf"), [Lit(Int 13)]))
               t (Syntax.App (Var(Id "printf"), [binOp "+" (sInt 13) (sInt 2)]))
               t (Syntax.App (Var(Id "printf"), [binOp "+" (binOp "+" (sInt 13) (sInt 2)) (sInt 5)]))
               t (LetIn(Syntax.Decl(Id("x"),Type.genType()), (sInt 3), Some <| (print [varId "x"])))
               t (sif (sBool true) (print [sInt 1]) (print [sInt 2]))
               t (sif (binOp "<" (sInt 2) (sInt 1)) (print [sInt 1]) (print [sInt 2]))
               p "if 1 < 2 then printf 1 else printf 2"
               p "let f (x:int) (y:int) : int = x + y"
            |]

    [<Test>]
    [<TestCaseSource(typeof<TCS>, "Data")>]
    let ``gen asm`` (data:Data) =
        let senv = ref (Env.newSymbolEnv())
        let env = ref (Env.newTypeEnv())
        let r = trial {
            let ast = match data with
            | Ast ast -> ast
            | Txt txt -> match Parser.parse txt with
                         | Pass ast -> ast
                         | e -> failwithf "%A" e
            let! t = ast |> Typing.typed env |> Trial.mapFailure (List.map Codegen.CodeGenError.TypingError)
            let ast = Typing.typed_deref ast
            return! Codegen.gen ast env senv "test.exe"
        }
        match r with
        | Pass _ -> ()
        | Fail e -> failwithf "ERROR %A" e
        | _ -> failwithf "ERROR %A" r