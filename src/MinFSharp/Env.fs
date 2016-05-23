namespace MinFSharp

module Env =
    open Identifier
    open Syntax
    type Symbol = Map<Identifier.t,Syntax.t>
    type Type = Map<Identifier.t,Type.t>
    let defs =
        [
         (Id "add", (Type.arrow [Type.Int;Type.Int;Type.Int]),
            FunDef([Decl(Id "x",Type.Int); Decl(Id "y", Type.Int)],
                   Ext(fun [Int(x); Int(y)] -> Int (x+y)), Type.Int))
         Id "(+)", Type.arrow [Type.Int;Type.Int;Type.Int], Var(Id "add")
         (Id "id", Type.arrow [Type.var "a";Type.var "a"],
             FunDef([Decl(Id "x", Type.Var <| Some "a")],
                    Ext(fun [_x] -> Var(Id "x")),
                    Type.Var <| Some "a"))
        ];
    let newTypeEnv : Type =
        defs |> List.map (fun (id,t,_def) -> (id, t)) |> Map.ofList
    let newSymbolEnv : Symbol =
        defs |> List.map (fun (id,_t,def) -> (id, def)) |> Map.ofList