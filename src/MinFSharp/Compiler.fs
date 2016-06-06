namespace MinFSharp

module ErrorDisplay =
    open System.IO
    open Chessie.ErrorHandling

    let printFilePos path lines (pos:Syntax.Pos) =
        trial {
            let! lines = lines
            let l = Array.length lines
            let slines = if pos.line <= l && pos.lineEnd <= l
                         then Array.sub lines (pos.line - 1) (pos.lineEnd - pos.line + 1) |> Some
                         else None
            match slines with
            | None -> return "Internal error"
            | Some slines ->
    //            if pos.line = pos.lineEnd then
                let f(i,l) =
                    let rSp i = String.replicate i " "
                    let rHi i = String.replicate i "^"
                    if pos.line <> pos.lineEnd then
                        match i with
                        | _ when i < pos.line-1 || pos.lineEnd < i -> [|l|] 
                        | _ when i = pos.line-1 ->
                            [| String.replicate pos.col " "; l |]
                    else
                        if i = pos.line-1 then
                            [| l;
                               (rSp (pos.col - 1)) + rHi (pos.colEnd - pos.col) + rSp (l.Length - pos.colEnd) |]
                        else [|l|]
//                        if i < pos.line-1 || pos.lineEnd < i then [|l|]
//                        else
//                            let hint = 
//                            [|l;"^"|]
                    
                let mlines = slines |> Array.indexed |> Array.collect f
                return sprintf "%s (%i,%i):\n%s" path pos.line pos.col (String.concat "\n" mlines)
        }

    let display path (e:Codegen.CodeGenError) =
        match e with
        | Codegen.TypingError (p,t) ->
            let lines = path |> Trial.Catch (File.ReadAllLines)
            match printFilePos path lines p with
            | Ok(s,_)  -> sprintf "%A\n%s" t s
            | _ -> sprintf "%A" e 
        | _ -> sprintf "%A" e 

module Compiler =
    open System.IO
    open Chessie.ErrorHandling
    open System.Reflection
    open System.Reflection.Emit
    open MinFSharp

    let compile_file (modl:ModuleBuilder) (m:MethodBuilder) (varEnv:Codegen.CliVarEnv ref) (path:string) =
        let path = System.IO.Path.GetFullPath path
        let senv = ref (Env.newSymbolEnv())
        let env = ref (Env.newTypeEnv())

        trial {
            let! txt = Trial.Catch File.ReadAllText path |> Trial.mapFailure (List.map Codegen.CodeGenError.Exception)
            let doc = modl.DefineDocument(path, System.Guid.Empty, System.Guid.Empty, System.Guid.Empty);
//            let doc = Document(path)
//            doc.Language <- DocumentLanguage.Other
//            doc.Type <- DocumentType.Text
            let pos,ast = match Parser.parse txt with
                          | Pass x -> x
                          | e -> failwithf "%A" e
            let! t = ast |> Typing.typed env |> Trial.mapFailure (List.map Codegen.CodeGenError.TypingError)
            let ast = Typing.typed_deref t ast

            let il = m.GetILGenerator()
            Codegen.seqPoint doc pos il
            il.Emit(OpCodes.Nop)
            return! Codegen.genMethodBody doc m senv varEnv (pos,ast)
        } |> Trial.failureTee (List.iter ((printf "%s") << ErrorDisplay.display path))
    let compile (assemblyPath:string) (files:string list) =
        let assemblyPath = System.IO.Path.GetFullPath assemblyPath
        let name = System.IO.Path.GetFileNameWithoutExtension assemblyPath
        let exeName = name + ".exe"
        let def:AssemblyBuilder = Codegen.createAssembly name (System.IO.Path.GetDirectoryName assemblyPath)

//        let tVoid = def.MainModule.TypeSystem.Void
        let modl = def.DefineDynamicModule(exeName, true);
        let tProgram = modl.DefineType(name + ".Program", TypeAttributes.Public ||| TypeAttributes.Class ||| TypeAttributes.BeforeFieldInit)
//        tProgram.BaseType <- def.MainModule.TypeSystem.Object
//        def.MainModule.Types.Add tProgram

        let mMain = tProgram.DefineMethod("Main", MethodAttributes.HideBySig ||| MethodAttributes.Static ||| MethodAttributes.Public, typeof<System.Void>, [| typeof<string array> |])
        mMain.DefineParameter(1, ParameterAttributes.None, "args") |> ignore
//        tProgram.Methods.Add mMain

        let varEnv : Codegen.CliVarEnv ref = ref Map.empty

        trial {
            let! _ = files |> List.map (compile_file modl mMain varEnv) |> Trial.collect
//            let parameters = WriterParameters()
//            parameters.WriteSymbols <- true
//            parameters.SymbolWriterProvider <- Pdb.PdbWriterProvider()
            tProgram.CreateType() |> ignore
            def.SetEntryPoint(mMain, PEFileKinds.ConsoleApplication)

            let ch f e = Path.ChangeExtension(f, e)
            let cd = Directory.GetCurrentDirectory()
            def.Save(exeName)
            return ()
        }