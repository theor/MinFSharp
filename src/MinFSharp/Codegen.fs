namespace MinFSharp

//open System.Reflection.Emit
open System.Reflection
open Mono.Cecil
open Mono.Cecil.Cil
open MinFSharp.Syntax
open Chessie.ErrorHandling

module Codegen =
    let tr<'a> (def:AssemblyDefinition) = def.MainModule.Import(typeof<'a>)
    let mr<'a> (m:string) (args:System.Type array) (def:AssemblyDefinition) = def.MainModule.Import(typeof<'a>.GetMethod(m, args))

    type Debuggable = System.Diagnostics.DebuggableAttribute
    type Mode = System.Diagnostics.DebuggableAttribute.DebuggingModes

    let createAssembly name =
        let asmName = AssemblyNameDefinition(name, System.Version(1, 0,0,0))
        let def = Mono.Cecil.AssemblyDefinition.CreateAssembly(asmName,name, ModuleKind.Console)

        let x = (typeof<AssemblyTitleAttribute>.GetConstructors() |> Array.head)
        let attr = CustomAttribute(def.MainModule.Import x)
        attr.ConstructorArguments.Add(CustomAttributeArgument(def.MainModule.TypeSystem.String, "test"))
        def.CustomAttributes.Add(attr)

        let dbgc = typeof<Debuggable>.GetConstructor([|typeof<Mode>|])
        let dbgArg = CustomAttributeArgument(def.MainModule.Import(typeof<Mode>), Mode.Default ||| Mode.DisableOptimizations ||| Mode.IgnoreSymbolStoreSequencePoints)
        let dbg = CustomAttribute(def.MainModule.Import dbgc)
        dbg.ConstructorArguments.Add(dbgArg)
        def.CustomAttributes.Add(dbg)

        def

    type CodeGenError =
    | NotFound of Identifier.t
    | TypingError of Typing.TypingError
    | UnknownType of Type.t
    | Exception of System.Exception

    type CliVar =
    | Local of VariableDefinition
    | Arg of int
    | Method of MethodReference
    type CliVarEnv = Map<Identifier.t, CliVar>

    let inline (<!>) (il:ILProcessor) op = il.Append <| il.Create(op); il
    let inline (<!!>) (il:ILProcessor) op = il.Append <| op; il

    let seqPoint doc (pos:Syntax.Pos) (instr:Instruction) =
        let sp = SequencePoint(doc)
        sp.StartLine <- int(pos.line)
        sp.EndLine <- int(pos.line)
        sp.StartColumn <- int(pos.col)
        sp.EndColumn <- int(pos.col) + 1
        instr.SequencePoint <- sp
        instr


    let rec deref (ast:Syntax.t) (senv:Env.Symbol ref) =
        match ast with
        | Var id -> match Map.tryFind id !senv with
                    | None -> fail (NotFound id)
                    | Some x -> deref x senv
        | _ -> ok ast

    let typeToCliType (def:ModuleDefinition) t =
        match t with
        | Type.Int -> ok def.TypeSystem.Int32
        | Type.Bool -> ok def.TypeSystem.Boolean
        | Type.Float -> ok def.TypeSystem.Single
        | _ -> fail (UnknownType t)

    let rec funCall (doc:Document) il fu senv varEnv args =
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
                    | Ext(Opcode o)-> il.Append <| il.Create o
                    | Ext(Ext.Method me) -> il.Append <| il.Create(OpCodes.Call, il.Body.Method.Module.Import me)
                    | Body _b ->
                        match !varEnv |> Map.tryFind fid with
                        | Some(Method methodRef) ->
                            il.Append <| il.Create(OpCodes.Call, methodRef)
                | _ -> failwith "ASDASD"
        }

    and genAst (doc:Document) (il:ILProcessor) (senv:Env.Symbol ref) (varEnv:CliVarEnv ref) ast =
        let sp = seqPoint doc
        match ast with
        | Lit l -> match l with
                   | Int i -> il.Create(OpCodes.Ldc_I4, i) |> il.Append |> ok
                   | Float f -> il.Create(OpCodes.Ldc_R4, f) |> il.Append |> ok
                   | Bool b -> il.Create(OpCodes.Ldc_I4, if b then 1 else 0) |> il.Append |> ok
                   | _ -> failwithf "Lit not implemented yet: %A" l
        | BinOp(op, (_lp,l), (_rp,r)) ->
            funCall doc il (Var(opId op)) senv varEnv [l;r]
        | App(fu, args) -> funCall doc il fu senv varEnv args
        | LetIn(Decl(Identifier.Id id as fid, _vt), (FunDef(args,body,vt) as fd), scope) ->
            trial {
                senv := !senv |> Map.add fid fd
                let! cliVt = typeToCliType il.Body.Method.Module vt
                let m = MethodDefinition(id, MethodAttributes.Static ||| MethodAttributes.Public, cliVt)
                varEnv := !varEnv |> Map.add fid (Method (m.GetElementMethod()))

                let senv = args |> List.fold (fun e (Decl(id, _t)) -> Map.add id (Var id) e) !senv |> ref
                varEnv := args |> List.indexed |> List.fold (fun e (i, Decl(id, _t)) ->  Map.add id (Arg i) e) !varEnv
                let declTy = il.Body.Method.DeclaringType
                let! targs = args
                             |> List.map (fun (Decl(Identifier.Id id, t)) ->
                                        trial {
                                            let! cliT = typeToCliType il.Body.Method.Module t
                                            return ParameterDefinition(id, ParameterAttributes.None, cliT)
                                        })
                             |> Trial.collect
                targs |> List.iter (fun a -> m.Parameters.Add(a))
                declTy.Methods.Add m
                match body with
                | FBody.Body b -> do! genMethodBody doc m senv varEnv b
                | _ -> failwithf "Should be standard method body"
                match scope with
                | None -> return ()
                | Some scope -> return! genAst doc il senv varEnv scope
            }
        | LetIn(Decl(id, vt), value, scope) ->
            trial {
                let! cliVt = typeToCliType il.Body.Method.Module vt
                let var = VariableDefinition cliVt
                varEnv := Map.add id (Local var) !varEnv
                il.Body.Variables.Add var
                let! _ = genAst doc il senv varEnv value
                il.Emit(OpCodes.Stloc, var)
//                il.Emit(OpCodes.Ldloc, var)
                match scope with
                | None -> return ()
                | Some scope -> return! genAst doc il senv varEnv scope
            }
        | Var(id) ->
            match Map.tryFind id !varEnv with
            | None -> fail (NotFound id)
            | Some(Local vd) -> il.Create(OpCodes.Ldloc, vd) |> il.Append |> ok
            | Some(Arg a) -> il.Create(OpCodes.Ldarg, a) |> il.Append |> ok
        | If((pcond, cond),(pthen, ethen), (pelse, eelse)) ->
            trial {
                let! _ = genAst doc il senv varEnv cond

                let nopElse = il.Create(OpCodes.Nop) |> sp pelse
                il.Create(OpCodes.Brfalse, nopElse) |> il.Append
                let nopEnd = il.Create(OpCodes.Nop)
                let! _ = genAst doc il senv varEnv ethen
                il.Create(OpCodes.Br, nopEnd) |> il.Append
                il.Append nopElse
                let! _ = genAst doc il senv varEnv eelse
                il.Append nopEnd
                return ()
            }
        | FunDef(_args,_body,_ret) -> failwith "NO FUN DEF"
        | Seq s ->
            trial {
                let _ = s |> List.map (snd >> (genAst doc il senv varEnv)) |> Trial.collect
                return ()
            }
        | Internal(Ignore ast) ->
            trial {
                let! _ = genAst doc il senv varEnv ast
                return il.Append <| il.Create OpCodes.Pop
            }

        | _ -> failwithf "Not implemented yet: %A" ast

    and genMethod () = ok ()

    and genMethodBody (doc:Document) (m:MethodDefinition) (senv:Env.Symbol ref) (varEnv:CliVarEnv ref) (ast:Syntax.t) =
        let il = m.Body.GetILProcessor()
        trial {
            let! a = genAst doc il senv varEnv ast
            return il.Create OpCodes.Ret |> il.Append
        }
        
    let gen (ast:Syntax.t) (tenv:Env.Type ref) (senv:Env.Symbol ref) (path:string) =
        let varEnv : CliVarEnv ref = ref Map.empty
        let name = System.IO.Path.GetFileNameWithoutExtension path

        let def = createAssembly name

        let doc = Document(System.IO.Path.ChangeExtension(path, "ml"))

        let tVoid = def.MainModule.TypeSystem.Void

        let tProgram = TypeDefinition(name, "Program", TypeAttributes.BeforeFieldInit)
        tProgram.BaseType <- def.MainModule.TypeSystem.Object
        def.MainModule.Types.Add tProgram

        let mMain = MethodDefinition("Main", MethodAttributes.Static ||| MethodAttributes.Public ||| MethodAttributes.HideBySig, tVoid)
        mMain.Parameters.Add(ParameterDefinition(def |> tr<string array>))
        def.MainModule.EntryPoint <- mMain
        tProgram.Methods.Add mMain
        trial {
            let! _ = genMethodBody doc mMain senv (varEnv) ast
            let parameters = WriterParameters()
            parameters.WriteSymbols <- true
            parameters.SymbolWriterProvider <- Pdb.PdbWriterProvider()

            def.Write(path, parameters)
            return ()
        }
