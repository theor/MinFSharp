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

module Identifier =
    type t = Id of string

module Syntax =

    [<CustomEquality;NoComparison>]
    type FBody<'U> = | Body of t<'U> | Ext of (t<'U> list -> t<'U>)
    with
        override x.Equals(_yobj) =
            true //TODO: FIXME
        override x.GetHashCode() = 0
//    and Op = Lt | Gt | Eq | Ne
    and Op = string
    and t<'U> =
    | Unit
    | Bool of bool
    | Int of int
    | Float of float
    | BinOp of Op * t<'U> * t<'U>
    | Let of (Identifier.t * Type.t) * t<'U>
    | LetIn of (Identifier.t * Type.t) * t<'U> * t<'U>
    | If of t<'U> * t<'U> * t<'U>
    | Var of Identifier.t
    | FunDef of (Identifier.t * Type.t) list * FBody<'U>
    | App of t<'U> * t<'U> list
    with
        override x.ToString() = sprintf "%A" x

    let appId s args = App(Var(Identifier.Id s), args)

module Env =
    open Identifier
    type t<'U> = Map<Identifier.t,Syntax.t<'U>>
    let newEnv<'U> =
        [(Id "add"), (Syntax.FunDef([Id "x",Type.Int; Id "y", Type.Int],
                                        Syntax.Ext(fun [Syntax.Int x; Syntax.Int y] -> Syntax.Int (x+y))))
        ] |> Map.ofList

module Typing =
    open Chessie.ErrorHandling
    type TypingError = UnknownSymbol of Identifier.t | TypeMismatch
    type TypingResult = Result<Type.t, TypingError>
    let rec typing env a : TypingResult =
        match a with
        | Syntax.Unit -> ok Type.Unit
        | Syntax.Bool(_) -> ok Type.Bool
        | Syntax.Int(_) -> ok Type.Int
        | Syntax.Float(_) -> ok Type.Float
        | Syntax.LetIn((vid,_), vval, e) ->
            trial {
                let! tVal = typing env vval
                let nEnv = env |> Map.add vid tVal
                return! typing nEnv e
            }
        | Syntax.Var(vid) ->
            Map.tryFind vid env |> failIfNone (UnknownSymbol(vid))
        | Syntax.FunDef(_, _) -> failwith "Not implemented yet"
        | Syntax.App(_, _) -> failwith "Not implemented yet"

module Interpreter =
    open Syntax
    open Chessie.ErrorHandling
    type EvalError = | AppNotFound
    type EvalResult<'U> = Result<Syntax.t<'U>,EvalError>
    let rec eval<'U> (e:Env.t<'U>)(a:Syntax.t<'U>) : EvalResult<'U> =
        match a with
        | Unit -> ok Unit
        | Bool(_) | Int(_) | Float(_) -> ok a
        | LetIn((id,_ty), value, body) -> eval (e |> Map.add id value) body
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
        | FunDef(_fargs, _body) -> ok a
        | _ -> failwith "Not implemented yet"