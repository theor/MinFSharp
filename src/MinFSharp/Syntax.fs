﻿namespace MinFSharp

module Identifier =
    type t = Id of string

module Syntax =
    [<CustomEquality;NoComparison>]
    type FBody<'U> = | Body of t<'U> | Ext of (t<'U> list -> t<'U>)
    with
        override x.Equals(yobj) =
            match yobj with
            | :? FBody<'U> as y ->
                match x,y with
                | Ext ex, Ext ey -> System.Object.ReferenceEquals(ex, ey)
                | Body bx, Body by -> bx = by
                | _,_ -> false
            | _ -> false
        override x.GetHashCode() = 0
//    and Op = Lt | Gt | Eq | Ne
    and Op = string
    and t<'U> =
    | Unit
    | Bool of bool
    | Int of int
    | Float of float
    | BinOp of Op * t<'U> * t<'U>
//    | Let of (Identifier.t * Type.t) * t<'U>
    | LetIn of (Identifier.t * Type.t) * t<'U> * (t<'U> option)
    | If of t<'U> * t<'U> * t<'U>
    | Var of Identifier.t
    | FunDef of (Identifier.t * Type.t) list * FBody<'U> * Type.t
    | App of t<'U> * t<'U> list
    | Seq of t<'U> list
    with
        override x.ToString() = sprintf "%A" x

    let opName (o:Op) = sprintf "(%s)" o

    let varId s = Var(Identifier.Id s)
    let appId s args = App(Var(Identifier.Id s), args)

    let map f s =
        match s with
        | Unit | Bool(_) | Int(_) | Float(_) | Var(_) -> f s
        | BinOp(op, l, r) -> f (BinOp(op, f l, f r))
        | LetIn((id,t), eval, ein) -> f (LetIn((id, t), f eval, Option.map f ein))
        | If(cond, ethen, eelse) -> f (If(f cond, f ethen, f eelse))
        | FunDef(args, Body body, ret) -> f(FunDef(args, f body |> Body, ret))
        | FunDef(args, Ext ext, ret) -> f(FunDef(args, Ext ext, ret))
        | App(fu, args) -> f(App(f fu, args |> List.map f))
        | Seq stmts -> f(Seq(stmts |> List.map f))