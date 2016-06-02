namespace MinFSharp

module Compiler =
    open System.IO
    open Chessie.ErrorHandling
    open Mono.Cecil
    open Mono.Cecil.Cil
    open MinFSharp

    let compile_file (m:MethodDefinition) (varEnv:Codegen.CliVarEnv ref) (path:string) =
        let path = System.IO.Path.GetFullPath path
        let senv = ref (Env.newSymbolEnv())
        let env = ref (Env.newTypeEnv())

        trial {
            let! txt = Trial.Catch File.ReadAllText path |> Trial.mapFailure (List.map Codegen.CodeGenError.Exception)
            let doc = Document(path)
            doc.Language <- DocumentLanguage.Other
            doc.Type <- DocumentType.Text
            let ast = match Parser.parse txt with
                      | Pass ast -> ast
                      | e -> failwithf "%A" e
            let! t = ast |> Typing.typed env |> Trial.mapFailure (List.map Codegen.CodeGenError.TypingError)
            let ast = Typing.typed_deref t ast
            return! Codegen.genMethodBody doc m senv varEnv ast
        }
    let compile (assemblyPath:string) (files:string list) =
        let assemblyPath = System.IO.Path.GetFullPath assemblyPath
        let name = System.IO.Path.GetFileNameWithoutExtension assemblyPath
        let def = Codegen.createAssembly name

        let tVoid = def.MainModule.TypeSystem.Void

        let tProgram = TypeDefinition(name, "Program", TypeAttributes.BeforeFieldInit)
        tProgram.BaseType <- def.MainModule.TypeSystem.Object
        def.MainModule.Types.Add tProgram

        let mMain = MethodDefinition("Main", MethodAttributes.Static ||| MethodAttributes.Public ||| MethodAttributes.HideBySig, tVoid)
        mMain.Parameters.Add(ParameterDefinition(def |> Codegen.tr<string array>))
        def.MainModule.EntryPoint <- mMain
        tProgram.Methods.Add mMain

        let varEnv : Codegen.CliVarEnv ref = ref Map.empty

        trial {
            let! _ = files |> List.map (compile_file mMain varEnv) |> Trial.collect
            let parameters = WriterParameters()
            parameters.WriteSymbols <- true
            parameters.SymbolWriterProvider <- Pdb.PdbWriterProvider()

            def.Write(assemblyPath, parameters)
            return ()
        }