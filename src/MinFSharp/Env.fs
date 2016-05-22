namespace MinFSharp

module Env =
    open Identifier
    type t<'U> = Map<Identifier.t,Syntax.t<'U>>
    let newEnv<'U> : t<'U> =
        [(Id "add"), (Syntax.FunDef([Id "x",Type.Int; Id "y", Type.Int],
                                        Syntax.Ext(fun [Syntax.Int x; Syntax.Int y] -> Syntax.Int (x+y)), Type.Int))
         (Id "(+)", Syntax.Var(Id "add"))
         (Id "id", Syntax.FunDef([Id "x", Type.Var <| Some "a"],
                                 Syntax.Ext(fun [x] -> Syntax.Var(Id "x")),
                                 Type.Var <| Some "a"))
        ] |> Map.ofList