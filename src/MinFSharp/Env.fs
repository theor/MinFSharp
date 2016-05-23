namespace MinFSharp

module Env =
    open Identifier
    open Syntax
    type t = Map<Identifier.t,Syntax.t>
    let newEnv<'U when 'U : equality> : t =
        [(Id "add"), (FunDef([Decl(Id "x",Type.Int); Decl(Id "y", Type.Int)],
                                        Ext(fun [Int(x); Int(y)] ->
                                            Int (x+y)), Type.Int))
         (Id "(+)", Var(Id "add"))
         (Id "id", FunDef([Decl(Id "x", Type.Var <| Some "a")],
                                 Ext(fun [x] -> Var(Id "x")),
                                 Type.Var <| Some "a"))
        ] |> Map.ofList