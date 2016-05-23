namespace MinFSharp

module Env =
    open Identifier
    open Syntax
    type t = Map<Identifier.t,(Type.t*Syntax.t)>
    let newEnv<'U when 'U : equality> : t =
        [
         (Id "add", (Type.arrow [Type.Int;Type.Int;Type.Int]),
            FunDef([Decl(Id "x",Type.Int); Decl(Id "y", Type.Int)],
                   Ext(fun [Int(x); Int(y)] -> Int (x+y)), Type.Int))
         Id "(+)", Type.arrow [Type.Int;Type.Int;Type.Int], Var(Id "add")
         (Id "id", Type.arrow [Type.var "a";Type.var "a"],
             FunDef([Decl(Id "x", Type.Var <| Some "a")],
                    Ext(fun [x] -> Var(Id "x")),
                    Type.Var <| Some "a"))
        ] |> List.map (fun (id,t,def) -> (id, (t,def))) |> Map.ofList