namespace MinFSharp

/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
module Library =

  /// Returns 42
  ///
  /// ## Parameters
  ///  - `num` - whatever
  let hello _num = 42

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

    type t =
    | Unit
    | Bool of bool
    | Int of int
    | Float of float
    | Let of (Identifier.t * Type.t) * t * t
    | Var of Identifier.t
    | FunDef of (Identifier.t * Type.t) * t
    | App of t * t
module Env =
    type t = Map<Identifier.t,Syntax.t>
    let newEnv = Map.empty
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
        | App(fid, args) ->
            match eval e fid with
            | Ok ((FunDef ((argId,_argTy), fbody)), _) -> eval (Map.add argId args e) fbody
            | Ok _ -> fail AppNotFound
            | Bad(_e) -> fail AppNotFound
        | FunDef(_fid, _body) -> ok a
        | _ -> failwith "Not implemented yet"