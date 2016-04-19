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
    | Var of t option ref

module Identifier =
    type t = Id of string

module Syntax =

    [<CustomEquality;NoComparison>]
    type FBody = | Body of t | Ext of (t list -> t)
    with
        override x.Equals(_yobj) =
            true //TODO: FIXME
        override x.GetHashCode() = 0

    and t =
    | Unit
    | Bool of bool
    | Int of int
    | Float of float
    | Let of (Identifier.t * Type.t) * t * t
    | Var of Identifier.t
    | FunDef of (Identifier.t * Type.t) list * FBody
    | App of t * t list
    with
        override x.ToString() = sprintf "%A" x

module Env =
    open Identifier
    type t = Map<Identifier.t,Syntax.t>
    let newEnv =
        [(Id "add"), (Syntax.FunDef([Id "x",Type.Int; Id "y", Type.Int],
                                        Syntax.Ext(fun [Syntax.Int x; Syntax.Int y] -> Syntax.Int (x+y))))
        ] |> Map.ofList

module Interpreter =
    open Syntax
    open Chessie.ErrorHandling
    type EvalError = | AppNotFound
    type EvalResult = Result<Syntax.t,EvalError>
    let rec eval (e:Env.t)(a:Syntax.t) : EvalResult =
        match a with
        | Unit -> ok Unit
        | Bool(_) | Int(_) | Float(_) -> ok a
        | Let((id,_ty), value, body) -> eval (e |> Map.add id value) body
        | Var(id) -> eval e (Map.find id e)
        | App(fid, fparams) ->
            match eval e fid with
            | Ok ((FunDef (fargs, fbody)), _) ->
                match fbody with
                | Body b ->
                    let ne = List.zip fargs fparams |> List.fold (fun env ((ai,_aty),fp) -> Map.add ai fp env) e
                    eval ne b
                | Ext ext -> ok <| ext fparams
            | Ok _ -> fail AppNotFound
            | Bad(_e) -> fail AppNotFound
        | FunDef(_fid, _body) -> ok a
        | _ -> failwith "Not implemented yet"