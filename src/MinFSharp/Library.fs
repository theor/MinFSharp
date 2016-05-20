namespace MinFSharp

module Type =
    type t =
    | Unit
    | Bool
    | Int
    | Float
    | Fun of t list * t (* arguments are uncurried *)
    | Tuple of t list
    | Array of t
    | Var of t option
    with
        override x.ToString() =
            let tstr x = x.ToString()
            match x with
            | Fun(l, r) -> l @ [r] |> List.map tstr |> String.concat " -> "
            | Tuple(l) -> l |> List.map tstr |> String.concat " * "
            | Array(t) -> sprintf "%O array" t
            | Var(t) -> sprintf "'%O" t
            | _ -> sprintf "%A" x

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

module Env =
    open Identifier
    type t<'U> = Map<Identifier.t,Syntax.t<'U>>
    let newEnv<'U> : t<'U> =
        [(Id "add"), (Syntax.FunDef([Id "x",Type.Int; Id "y", Type.Int],
                                        Syntax.Ext(fun [Syntax.Int x; Syntax.Int y] -> Syntax.Int (x+y)), Type.Int))
         (Id "(+)", Syntax.Var(Id "add"))
        ] |> Map.ofList

module Typing =
    open Chessie.ErrorHandling
    open Chessie.ErrorHandling.Trial
    type TypingError = UnknownSymbol of Identifier.t | TypeMismatch
    type TypingResult = Result<Type.t, TypingError>
    type TypedAstResult<'U> = Result<Syntax.t<'U> * Type.t, TypingError>
    let rec typed<'U> (env:Env.t<'U>) x : TypedAstResult<'U> =
        match x with
        | Syntax.Unit -> ok (x, Type.Unit)
        | Syntax.Bool(_) -> ok (x, Type.Bool)
        | Syntax.Int(_) -> ok (x, Type.Int)
        | Syntax.Float(_) -> ok (x, Type.Float)
        | Syntax.BinOp(op, a, b) ->
            trial {
                let! ta, tya = typed env a
                let! tb, tyb = typed env b
                let opId = Syntax.opName op |> Identifier.Id
                match Map.tryFind opId env with
                | None -> return! fail (UnknownSymbol opId)
                | Some o ->
                    let! _top, tyop = typed env o
                    match tyop with
                    | Type.Fun([atya; atyb], tret) when atya = tya && atyb = tyb ->
                        return Syntax.BinOp(op, ta, tb), tret
                    | _ -> return! fail TypeMismatch
            }
        | Syntax.LetIn(_, _, _) -> failwith "Not implemented yet"
        | Syntax.If(_, _, _) -> failwith "Not implemented yet"
        | Syntax.Var(v) ->
            match Map.tryFind v env with
            | None -> fail (UnknownSymbol v)
            | Some vd -> typed env vd
        | Syntax.FunDef(args, body, ret) ->
            ok (x, Type.Fun(args |> List.map snd, ret))
//            trial {
//                let! tr, tyr = typed env body
//            }
        | Syntax.App(_, _) -> failwith "Not implemented yet"
        | Syntax.Seq(_) -> failwith "Not implemented yet"
    let rec typing env a : TypingResult =
        match a with
        | Syntax.Unit -> ok Type.Unit
        | Syntax.Bool(_) -> ok Type.Bool
        | Syntax.Int(_) -> ok Type.Int
        | Syntax.Float(_) -> ok Type.Float
        | Syntax.LetIn((_vid,_), vval,None) ->  typing env vval
        | Syntax.LetIn((vid,_), vval, Some e) ->
            trial {
                let! tVal = typing env vval
                let nEnv = env |> Map.add vid tVal
                return! typing nEnv e
            }
        | Syntax.Var(vid) ->
            Map.tryFind vid env |> failIfNone (UnknownSymbol(vid))
        | Syntax.FunDef(args, Syntax.FBody.Ext body, _ret) -> fail TypeMismatch
        | Syntax.FunDef(args, Syntax.FBody.Body body, _ret) ->
            trial {
                let targs = args |> List.map (snd)

                let! tvody = typing env body
                return Type.Fun(targs, tvody)
            }
        | Syntax.App(_, _) -> failwith "Not implemented yet"
        | Syntax.Seq l -> l |> List.map (typing env) |> Trial.collect |> Trial.bind (Seq.last >> ok)

module Interpreter =
    open Syntax
    open Chessie.ErrorHandling
    open Chessie.ErrorHandling.Trial
    type EvalError = | AppNotFound of Identifier.t | OpNotFound of string | ApplyNotFunction
    type EvalResult<'U> = Result<Syntax.t<'U>,EvalError>
    let rec eval<'U> (e:Env.t<'U>)(a:Syntax.t<'U>) : EvalResult<'U> =
        match a with
        | Unit -> ok Unit
        | Bool(_) | Int(_) | Float(_) -> ok a
        | LetIn((id,_ty), value, Some body) -> eval (e |> Map.add id value) body
        | Var(id) ->
            trial {
                let! def = (Map.tryFind id e) |> failIfNone (AppNotFound id)
                return! eval e def
            }
        | App(fid, fparams) ->
            match eval e fid with
            | Ok ((FunDef (fargs, fbody, _tret)), _) ->
                match fbody with
                | Body b ->
                    let ne = List.zip fargs fparams |> List.fold (fun env ((ai,_aty),fp) -> Map.add ai fp env) e
                    eval ne b
                | Ext ext -> ok <| ext fparams
            | Ok _ -> fail ApplyNotFunction
            | Bad(_e) -> fail _e.Head
        | FunDef(_fargs, _body, _tret) -> ok a
        | BinOp(op, l, r) ->
            let oid = (Identifier.Id <| sprintf "(%s)" op)
            eval e (App(Var oid, [l; r]))
        | LetIn(_, _, _) -> failwith "Not implemented yet"
        | If(eif, ethen, eelse) ->
            trial {
                let! rif = eval e eif
                return! if rif = Bool true then eval e ethen else eval e eelse
            }
        | Seq(_) -> failwith "Not implemented yet"
        //| _ -> failwith "Not implemented yet"