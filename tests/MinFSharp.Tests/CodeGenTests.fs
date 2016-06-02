namespace MinFSharp.Tests

module CodeGenTests =

    open System.Reflection
    open MinFSharp
    open MinFSharp.Syntax
    open MinFSharp.Identifier
    open NUnit.Framework
    open Chessie.ErrorHandling
    open FsUnitTyped

    let tidx = ref 0
    let print l = Syntax.App (Var(Id "printf"), l)
    type Data = Ast of Syntax.t | Txt of string
    let t (ast:Syntax.t) =
        tidx := !tidx + 1
        TestCaseData(Ast ast, !tidx).SetName(ast.ToString())
    let p (str) =
        tidx := !tidx + 1
        TestCaseData(Txt str, !tidx).SetName(str)
    let n name (tcd:TestCaseData) = tcd.SetName(name)

    type TCS() =
        static member Data() =
            [| t (Lit(Int 42))
               t (Syntax.App (Var(Id "printf"), [Lit(Int 13)]))
               t (Syntax.App (Var(Id "printf"), [binOp "+" (sInt 13) (sInt 2)]))
               t (Syntax.App (Var(Id "printf"), [binOp "+" (binOp "+" (sInt 13) (sInt 2)) (sInt 5)]))
               t (LetIn(Syntax.Decl(Id("x"),Type.genType()), (sInt 3), Some <| (print [varId "x"])))
               t (sif (sBool true) (print [sInt 1]) (print [sInt 2]))
               t (sif (binOp "<" (sInt 2) (sInt 1)) (print [sInt 1]) (print [sInt 2]))
               p "1"
               p "if 1 < 2 then printf 1 else printf 2"
               p "let f (x:int) (y:int) : int = x + y"
               p "let f (x:int) (y:int) : int = x + y in f 1 2"
               p "let f (x:int) (y:int) : int = x + y in printf (f 1 2); printf (f 2 3)"
               p "printf 1; printf 2"
            |]

    let (|Inner|_|) typ (exn:System.Exception) =
        match exn with
        | :? System.Reflection.TargetInvocationException as e ->
            match e.InnerException with
            | null -> None
            | x when x.GetType() = typ -> Some x
            | _ -> None
        | _ -> None
    let innerIs<'a when 'a :> System.Exception> (exn:System.Exception) =
        match exn :?> System.Reflection.TargetInvocationException with
        | null -> None
        | e -> if e.InnerException :? 'a then Some e.InnerException else None

    [<Test>]
    [<TestCaseSource(typeof<TCS>, "Data")>]
    let ``gen asm`` (data:Data) idx =
        if  not <| System.IO.Directory.Exists "tests" then ignore <| System.IO.Directory.CreateDirectory "tests"
        let path = System.IO.Path.Combine(System.IO.Path.GetFullPath "tests", sprintf "test-%i.exe" idx)
        printfn "path: %s" path
        try System.IO.File.Delete path with | e -> printfn "%A" e
        let senv = ref (Env.newSymbolEnv())
        let env = ref (Env.newTypeEnv())
        let r = trial {
            let ast = match data with
                      | Ast ast -> ast
                      | Txt txt -> match Parser.parse txt with
                                   | Pass ast -> ast
                                   | e -> failwithf "%A" e
            let! t = ast |> Typing.typed env |> Trial.mapFailure (List.map Codegen.CodeGenError.TypingError)
            let ast = Typing.typed_deref t ast
            return! Codegen.gen ast env senv path
        }
        match r with
        | Pass _ ->
            let typ = typeof<System.InvalidProgramException>
            try
                let a = Assembly.LoadFrom path
                let types = a.GetTypes()
                printf "%A" types
                let tProgram = a.GetType (sprintf "test-%i.Program" idx)
                let mMain = tProgram.GetMethod("Main")// BindingFlags.Static)
                printfn "%A" (tProgram.GetMethods())
                let res = mMain.Invoke(null, [|Array.empty<string>|])
                ()
            with
            | Inner typ e -> Assert.Fail "Invalid Program"
            | e -> printfn "%A" e; Assert.Inconclusive (sprintf "%A" (e.GetType().Name))
        | Fail e -> failwithf "ERROR %A" e
        | _ -> failwithf "ERROR %A" r

    type FileSource() =
        static member Data() =
            System.IO.Directory.EnumerateFiles("cases", "*.ml")
            |> Seq.map (fun x -> x)
            |> Seq.toArray

    [<Test>]
    [<TestCaseSource(typeof<FileSource>, "Data")>]
    let f file =
        match Compiler.compile (System.IO.Path.ChangeExtension(file, ".exe")) [file] with
        | _ -> ()