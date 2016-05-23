namespace MinFSharp

module Env =
    open Identifier
    open Syntax
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

    let defs =
        [
         (Id "add", (Type.arrow [Type.Int;Type.Int;Type.Int]),
            FunDef([Decl(Id "x",Type.Int); Decl(Id "y", Type.Int)],
                   Ext(fun [Int(x); Int(y)] -> Int (x+y)), Type.Int))
         Id "(+)", Type.arrow [Type.Int;Type.Int;Type.Int], Var(Id "add")
         (Id "id", Type.arrow [Type.Poly 0u; Type.Poly 0u],
             FunDef([Decl(Id "x", Type.Poly 0u)],
                    Ext(fun [_x] -> Var(Id "x")),
                    Type.Poly 0u))
        ];
    let newTypeEnv : Type =
        let types = defs |> List.map (fun (id,t,_def) -> (id, t)) |> Map.ofList
        { types = types; polytypeCount = 0u }
    let newSymbolEnv : Symbol =
        defs |> List.map (fun (id,_t,def) -> (id, def)) |> Map.ofList