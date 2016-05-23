namespace MinFSharp

module Identifier =
    type t = Id of string

module Syntax =
//    [<CustomEquality;NoComparison>]
    type Pos = {line:int64; col:int64}
    with
        static member from(p:FParsec.Position) = {line=p.Line; col=p.Column}
        static member from(l,c) = {line=l; col=c}
        static member zero = Pos.from(0L, 0L)
    let zeroPos = FParsec.Position(null, 0L, 0L, 0L)
    [<CustomEquality;NoComparison>]
    type FBody = | Body of body:t | Ext of ext:(t list -> t)
    with
        override x.Equals(yobj) =
            match yobj with
            | :? FBody as y ->
                match x,y with
                | Ext ex, Ext ey -> System.Object.ReferenceEquals(ex, ey)
                | Body bx, Body by -> bx = by
                | _,_ -> false
            | _ -> false
        override x.GetHashCode() = 0
//    and Op = Lt | Gt | Eq | Ne
    and Op = string
    and post = Pos * t
    and VarDecl = Decl of id:Identifier.t * ty:Type.t
    and t =
    | Unit
    | Bool of bool
    | Int of int
    | Float of float
    | BinOp of Op * post * post
    | LetIn of id:VarDecl * value:t * scope:(t option)
    | If of post * post * post
    | Var of Identifier.t
    | FunDef of args:(Identifier.t * Type.t) list * body:FBody * ty:Type.t
    | App of t * t list
    | Seq of post list
    with
        override x.ToString() = sprintf "%A" x

    let opName (o:Op) = sprintf "(%s)" o

    let varId s = Var(Identifier.Id s)
    let appId s args = App(Var(Identifier.Id s), args)

    let inline (@@) s (l:int64, c:int64) : post = (Pos.from(l, c), s)
    let inline (@=) s (p:Pos) : post = (p, s)
//    let inline (@@) s (l:int, c:int) : post = (Pos.from(int64 l, int64 c), s)
//    let map f s =
//        match s with
//        | Unit | Bool(_) | Int(_) | Float(_) | Var(_) -> f s
//        | BinOp(op, l, r) -> f (BinOp(op, f l, f r))
//        | LetIn((id,t), eval, ein) -> f (LetIn((id, t), f eval, Option.map f ein))
//        | If(cond, ethen, eelse) -> f (If(f cond, f ethen, f eelse))
//        | FunDef(args, Body body, ret) -> f(FunDef(args, f body |> Body, ret))
//        | FunDef(args, Ext ext, ret) -> f(FunDef(args, Ext ext, ret))
//        | App(fu, args) -> f(App(f fu, args |> List.map f))
//        | Seq stmts -> f(Seq(stmts |> List.map f))