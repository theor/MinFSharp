namespace MinFSharp

open System.Diagnostics.SymbolStore
open System.Reflection
open System.Reflection.Emit

open MinFSharp.Syntax
open Chessie.ErrorHandling

module Codegen =
//    let tr<'a> (def:AssemblyDefinition) = def.MainModule.Import(typeof<'a>)
//    let mr<'a> (m:string) (args:System.Type array) (def:AssemblyDefinition) = def.MainModule.Import(typeof<'a>.GetMethod(m, args))

    type Debuggable = System.Diagnostics.DebuggableAttribute
    type Mode = System.Diagnostics.DebuggableAttribute.DebuggingModes

    let createAssembly name (dir:string) =
        let asmName = AssemblyName(name)
        asmName.Version <- System.Version(1,0,0,0)
        let def = System.Threading.Thread.GetDomain().DefineDynamicAssembly(asmName, AssemblyBuilderAccess.Save, dir)
        let titleAttr = CustomAttributeBuilder((typeof<AssemblyTitleAttribute>.GetConstructors() |> Array.head), [|"test"|])
        def.SetCustomAttribute titleAttr
//        let def = Mono.Cecil.AssemblyDefinition.CreateAssembly(asmName,name, ModuleKind.Console)
//
//        let x = (typeof<AssemblyTitleAttribute>.GetConstructors() |> Array.head)
//        let attr = CustomAttribute(def.MainModule.Import x)
//        attr.ConstructorArguments.Add(CustomAttributeArgument(def.MainModule.TypeSystem.String, "test"))
//        def.CustomAttributes.Add(attr)
//
        let dbgc = typeof<Debuggable>.GetConstructor([|typeof<Mode>|])
        let dbgArg = CustomAttributeBuilder(dbgc, [| Mode.Default ||| Mode.DisableOptimizations ||| Mode.IgnoreSymbolStoreSequencePoints |])
        def.SetCustomAttribute dbgArg
//        let dbg = CustomAttribute(def.MainModule.Import dbgc)
//        dbg.ConstructorArguments.Add(dbgArg)
//        def.CustomAttributes.Add(dbg)

        def

    type CodeGenError =
    | NotFound of Identifier.t
    | TypingError of Typing.TypingError
    | UnknownType of Type.t
    | Exception of System.Exception

    type CliVar =
    | Local of LocalBuilder
    | Arg of int
    | Method of MethodBuilder
    type CliVarEnv = Map<Identifier.t, CliVar>

//    let inline (<!>) (il:ILProcessor) op = il.Append <| il.Emit(op); il
//    let inline (<!!>) (il:ILProcessor) op = il.Append <| op; il

    let seqPoint doc (pos:Syntax.Pos) (il:ILGenerator) =
        il.MarkSequencePoint(doc, pos.line, pos.col, pos.lineEnd, pos.colEnd)
//        let sp = SequencePoint(doc)
//        sp.StartLine <- int(pos.line)
//        sp.EndLine <- int(pos.line)
//        sp.StartColumn <- int(pos.col)
//        sp.EndColumn <- int(pos.col) + 1
//        instr.SequencePoint <- sp
        //il


    let rec deref (ast:Syntax.t) (senv:Env.Symbol ref) =
        match ast with
        | Var id -> match Map.tryFind id !senv with
                    | None -> fail (NotFound id)
                    | Some x -> deref x senv
        | _ -> ok ast

//    let typeToCliType (def:ModuleDefinition) t =
//        match t with
//        | Type.Int -> ok def.TypeSystem.Int32
//        | Type.Bool -> ok def.TypeSystem.Boolean
//        | Type.Float -> ok def.TypeSystem.Single
//        | _ -> fail (UnknownType t)

    let rec funCall (doc:ISymbolDocumentWriter) il ((posfu,fu):post) senv varEnv args =
        let rec deref_fun (id) (senv:Env.Symbol ref) =
            match Map.tryFind id !senv with
            | Some(Var id) -> deref_fun id senv
            | Some(FunDef(_,_,_) as fd) -> ok (id, fd)
            | _ -> fail (NotFound id)
        trial {
            match fu with
            | Var(fid) ->
                let! fid,f = deref_fun fid senv
                match f with
                | FunDef(_args, body, _ret) ->
                    let! _ = args |> List.map (genAst doc il senv varEnv) |> Trial.collect
                    match body with
                    | Ext(Opcode o)-> il.Emit o
                    | Ext(Ext.Method me) -> il.EmitCall(OpCodes.Call, me, [||])
                    | Body _b ->
                        match !varEnv |> Map.tryFind fid with
                        | Some(Method methodRef) ->
                           seqPoint doc posfu il
                           il.EmitCall(OpCodes.Call, methodRef, [||])
                | _ -> failwith "ASDASD"
        }

    and genAst (doc:ISymbolDocumentWriter) (il:ILGenerator) (senv:Env.Symbol ref) (varEnv:CliVarEnv ref) (pos,ast) : Result<unit,CodeGenError> =
        let sp = seqPoint doc
        match ast with
        | Lit l ->
            let instr = match l with
                        | Int i -> il.Emit(OpCodes.Ldc_I4, i)
                        | Float f -> il.Emit(OpCodes.Ldc_R4, f)
                        | Bool b -> il.Emit(OpCodes.Ldc_I4, if b then 1 else 0)
                        | _ -> failwithf "Lit not implemented yet: %A" l
            instr (*|> sp pos*) |> ok
        | BinOp(op, l, r) ->
            funCall doc il (pos, Var(opId op)) senv varEnv [l;r]
        | App(fu, args) -> funCall doc il fu senv varEnv args //FIXME
        | LetIn(Decl(Identifier.Id id as fid, _vt), (posDef, (FunDef(args,body,vt) as fd)), scope) ->
            ok()
//            trial {
//                senv := !senv |> Map.add fid fd
//                let! cliVt = typeToCliType il.Body.Method.Module vt
//                let m = MethodBuilder(id, MethodAttributes.Static ||| MethodAttributes.Public, cliVt)
//                varEnv := !varEnv |> Map.add fid (Method (m.GetElementMethod()))
//
//                let senv = args |> List.fold (fun e (Decl(id, _t)) -> Map.add id (Var id) e) !senv |> ref
//                varEnv := args |> List.indexed |> List.fold (fun e (i, Decl(id, _t)) ->  Map.add id (Arg i) e) !varEnv
//                let declTy = il.Body.Method.DeclaringType
//                let! targs = args
//                             |> List.map (fun (Decl(Identifier.Id id, t)) ->
//                                        trial {
//                                            let! cliT = typeToCliType il.Body.Method.Module t
//                                            return ParameterDefinition(id, ParameterAttributes.None, cliT)
//                                        })
//                             |> Trial.collect
//                targs |> List.iter (fun a -> m.Parameters.Add(a))
//                declTy.Methods.Add m
//                match body with
//                | FBody.Body b -> do! genMethodBody doc m senv varEnv b
//                | _ -> failwithf "Should be standard method body"
//                match scope with
//                | None -> return ()
//                | Some scope -> return! genAst doc il senv varEnv scope
//            }
        | LetIn(Decl(id, vt), value, scope) ->
//            trial {
//                let! cliVt = typeToCliType il.Body.Method.Module vt
//                let var = VariableDefinition cliVt
//                varEnv := Map.add id (Local var) !varEnv
//                il.Body.Variables.Add var
//                let! _ = genAst doc il senv varEnv value
//                il.Emit(OpCodes.Stloc, var)
////                il.Emit(OpCodes.Ldloc, var)
//                match scope with
//                | None -> return ()
//                | Some scope -> return! genAst doc il senv varEnv scope
//            }
            ok()
        | Var(id) ->
            match Map.tryFind id !varEnv with
            | None -> fail (NotFound id)
            | Some(Local vd) -> il.Emit(OpCodes.Ldloc, vd) |> ok
            | Some(Arg a) -> il.Emit(OpCodes.Ldarg, a) |> ok
        | If(cond,ethen,eelse) ->
            trial {
                let! _ = genAst doc il senv varEnv cond
                let lElse = il.DefineLabel()
                let lEnd = il.DefineLabel()
                sp (fst cond) il
                il.Emit(OpCodes.Brfalse, lElse)
                let! _ = genAst doc il senv varEnv ethen
                il.Emit(OpCodes.Br, lEnd)
                il.MarkLabel lElse
                let! _ = genAst doc il senv varEnv eelse
                il.MarkLabel lEnd
                return ()
            }
        | FunDef(_args,_body,_ret) -> failwith "NO FUN DEF"
        | Seq s ->
            trial {
                let _ = s |> List.map (genAst doc il senv varEnv) |> Trial.collect
                return ()
            }
        | Internal(Ignore ast) ->
            trial {
                let! _ = genAst doc il senv varEnv (pos,ast)
                sp pos il
                return il.Emit OpCodes.Pop
            }

        | _ -> failwithf "Not implemented yet: %A" ast

    and genMethod () = ok ()

    and genMethodBody (doc:ISymbolDocumentWriter) (m:MethodBuilder) (senv:Env.Symbol ref) (varEnv:CliVarEnv ref) ((pos,ast):Syntax.post) =
        let il = m.GetILGenerator()
        trial {
            let! a = genAst doc il senv varEnv (pos,ast)
            return il.Emit OpCodes.Ret
        }

//    let gen ((pos,ast):Syntax.post) (tenv:Env.Type ref) (senv:Env.Symbol ref) (path:string) =
//        let varEnv : CliVarEnv ref = ref Map.empty
//        let name = System.IO.Path.GetFileNameWithoutExtension path
//
//        let def = createAssembly name
//
//        let doc = Document(System.IO.Path.ChangeExtension(path, "ml"))
//
//        let tVoid = def.MainModule.TypeSystem.Void
//
//        let tProgram = TypeDefinition(name, "Program", TypeAttributes.BeforeFieldInit)
//        tProgram.BaseType <- def.MainModule.TypeSystem.Object
//        def.MainModule.Types.Add tProgram
//
//        let mMain = MethodDefinition("Main", MethodAttributes.Static ||| MethodAttributes.Public ||| MethodAttributes.HideBySig, tVoid)
//        mMain.Parameters.Add(ParameterDefinition(def |> tr<string array>))
//        def.MainModule.EntryPoint <- mMain
//        tProgram.Methods.Add mMain
//        trial {
//            let! _ = genMethodBody doc mMain senv (varEnv) (pos,ast)
//            let parameters = WriterParameters()
//            parameters.WriteSymbols <- true
//            parameters.SymbolWriterProvider <- Pdb.PdbWriterProvider()
//
//            def.Write(path, parameters)
//            return ()
//        }
