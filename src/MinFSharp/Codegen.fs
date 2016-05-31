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
    
    let createAssembly name =
        let asmName = AssemblyNameDefinition(name, System.Version(1, 0,0,0))
        let def = Mono.Cecil.AssemblyDefinition.CreateAssembly(asmName,name, ModuleKind.Console)

        let x = (typeof<AssemblyTitleAttribute>.GetConstructors() |> Array.head)
        let attr = CustomAttribute(def.MainModule.Import x)
        attr.ConstructorArguments.Add(CustomAttributeArgument(def.MainModule.TypeSystem.String, "test"))
        def.CustomAttributes.Add(attr)
        def

    type CodeGenError = NotFound of Identifier.t

    let inline (<!>) (il:ILProcessor) op = il.Append <| il.Create(op); il
    let inline (<!!>) (il:ILProcessor) op = il.Append <| op; il

    let rec deref (ast:Syntax.t) (senv:Env.Symbol ref) =
        match ast with
        | Var id -> match Map.tryFind id !senv with
                    | None -> fail (NotFound id)
                    | Some x -> deref x senv
        | _ -> ok ast

    let rec funCall il fu senv varEnv args =
        trial {
            let! f = deref fu senv
            match f with
            | FunDef(_args, Ext m, _ret) ->
                let! _ = args |> List.map (genAst il senv varEnv) |> Trial.collect
                match m with
                | Opcode o -> il.Append <| il.Create o
                | Method me -> il.Append <| il.Create(OpCodes.Call, il.Body.Method.Module.Import me)
            | _ -> failwith "ASDASD"
        }

    and genAst (il:ILProcessor) (senv:Env.Symbol ref) (varEnv) ast =
        match ast with
        | Lit l -> match l with
                   | Int i -> il.Create(OpCodes.Ldc_I4, i) |> il.Append |> ok
                   | Float f -> il.Create(OpCodes.Ldc_R4, f) |> il.Append |> ok
                   | Bool b -> il.Create(OpCodes.Ldc_I4, if b then 1 else 0) |> il.Append |> ok
                   | _ -> failwithf "Lit not implemented yet: %A" l
        | BinOp(op, (_lp,l), (_rp,r)) ->
            funCall il (Var(opId op)) senv varEnv [l;r]            
        | App(fu, args) -> funCall il fu senv varEnv args
        | LetIn(Decl(id, vt), value, scope) ->
            let var = VariableDefinition(il.Body.Method.Module.TypeSystem.Int32)
            varEnv := Map.add id var !varEnv
            il.Body.Variables.Add var
            trial {
                let! _ = genAst il senv varEnv value
                il.Emit(OpCodes.Stloc, var)
//                il.Emit(OpCodes.Ldloc, var)
                match scope with
                | None -> return ()
                | Some scope -> return! genAst il senv varEnv scope
            }
        | Var(id) ->
            match Map.tryFind id !varEnv with
            | None -> fail (NotFound id)
            | Some vd -> il.Create(OpCodes.Ldloc, vd) |> il.Append |> ok
        | If((_pcond, cond),(_pthen, ethen), (_pelse, eelse)) ->
            trial {
                let! _ = genAst il senv varEnv cond
                let nopElse = il.Create(OpCodes.Nop)
                il.Create(OpCodes.Brfalse, nopElse) |> il.Append
                let nopEnd = il.Create(OpCodes.Nop)
                let! _ = genAst il senv varEnv ethen
                il.Create(OpCodes.Br, nopEnd) |> il.Append
                il.Append nopElse
                let! _ = genAst il senv varEnv eelse
                il.Append nopEnd
                return ()
            }
        | _ -> failwithf "Not implemented yet: %A" ast
    let genMethodBody (m:MethodDefinition) (senv:Env.Symbol ref) varEnv (ast:Syntax.t) =
        let il = m.Body.GetILProcessor()
        genAst il senv varEnv ast
        il.Create OpCodes.Ret |> il.Append

    let gen (ast:Syntax.t) (tenv:Env.Type ref) (senv:Env.Symbol ref) (path:string) =
        let varEnv = ref Map.empty
        let name = System.IO.Path.GetFileNameWithoutExtension path
        
        let def = createAssembly name

        let tVoid = def.MainModule.TypeSystem.Void

        let tProgram = TypeDefinition(name, "Program", TypeAttributes.BeforeFieldInit)
        tProgram.BaseType <- def.MainModule.TypeSystem.Object
        def.MainModule.Types.Add tProgram

        let mMain = MethodDefinition("Main", MethodAttributes.Static ||| MethodAttributes.Public ||| MethodAttributes.HideBySig, tVoid)
        mMain.Parameters.Add(ParameterDefinition(def |> tr<string array>))
        def.MainModule.EntryPoint <- mMain
        tProgram.Methods.Add mMain

        genMethodBody mMain senv (varEnv) ast
//        let il = mMain.Body.GetILProcessor()
//        let writeLine = def |> mr<System.Console> "WriteLine" [| typeof<string> |]
//        il <!> OpCodes.Ldarg_0
//           <!> OpCodes.Ldc_I4_0
//           <!> OpCodes.Ldelem_I
//           <!!> il.Create (OpCodes.Call, writeLine)
//           <!> OpCodes.Ret

        def.Write path
        ()
