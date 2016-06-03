namespace MinFSharp

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
            return! Codegen.genMethodBody doc m senv varEnv (pos,ast)
        }
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