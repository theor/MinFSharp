namespace MinFSharp

//open System.Reflection.Emit
open System.Reflection
open Mono.Cecil
open Mono.Cecil.Cil
open MinFSharp.Syntax

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

    let inline (<!>) (il:ILProcessor) op = il.Append <| il.Create(op); il
    let inline (<!!>) (il:ILProcessor) op = il.Append <| op; il

    let gen (ast:Syntax.t) (path:string) =
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

        let il = mMain.Body.GetILProcessor()
        let writeLine = def |> mr<System.Console> "WriteLine" [| typeof<string> |]
        il <!> OpCodes.Ldarg_0
           <!> OpCodes.Ldc_I4_0
           <!> OpCodes.Ldelem_I
           <!!> il.Create (OpCodes.Call, writeLine)
           <!> OpCodes.Ret

        def.Write path
        ()
