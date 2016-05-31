namespace MinFSharp

module Env =
    open Identifier
    open Syntax
    open Mono.Cecil.Cil
    type Symbol = Map<Identifier.t,Syntax.t>
    type Type = private { types:Map<Identifier.t,Type.t>
                          mutable polytypeCount:uint32 }
    with
        member x.tryFind = x.types.TryFind
    let nextPolyType e =
        e := { !e with polytypeCount = (!e).polytypeCount  + 1u }
        Type.Poly ((!e).polytypeCount - 1u)
    let add id t e = { e with types = Map.add id t e.types }
    let find id e = Map.find id e.types

    let extOpCode id funtype op =
        let args = List.take (List.length funtype - 1) funtype
                   |> List.map (fun x -> Decl(Id "_", x))
        (id, (Type.arrow funtype), FunDef(args,Ext(Opcode op), funtype |> Seq.last))

    let defs() =
        [
         extOpCode "(=)" [Type.Int;Type.Int;Type.Bool] OpCodes.Ceq
//         extOpCode "(<>)" [Type.Int;Type.Int;Type.Bool] OpCodes.
         extOpCode "(<)" [Type.Int;Type.Int;Type.Bool] OpCodes.Clt
         extOpCode "(>)" [Type.Int;Type.Int;Type.Bool] OpCodes.Cgt
         extOpCode "(+)" [Type.Int;Type.Int;Type.Int] OpCodes.Add
         "add", Type.arrow [Type.Int;Type.Int;Type.Int], Var(Id "(+)")
         extOpCode "id" [Type.poly 0u; Type.poly 0u] OpCodes.Nop
           
         "printf", Type.arrow [Type.Int;Type.Unit], FunDef([Decl(Id "x", Type.Int)], Ext(Method(typeof<System.Console>.GetMethod("WriteLine", [|typeof<int>|]))),Type.Unit)
        ]
    let newTypeEnv() : Type =
        let types = defs() |> List.map (fun (id,t,_def) -> (Id id, t)) |> Map.ofList
        { types = types; polytypeCount = 0u }
    let newSymbolEnv() : Symbol =
        defs() |> List.map (fun (id,_t,def) -> (Id id, def)) |> Map.ofList