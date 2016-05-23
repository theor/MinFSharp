namespace MinFSharp

module Interpreter =
    open Syntax
    open Chessie.ErrorHandling
    open Chessie.ErrorHandling.Trial
    type EvalError = | AppNotFound of Identifier.t | OpNotFound of string | ApplyNotFunction
    type EvalResult = Result<Syntax.t,EvalError>
    let rec eval (e:Env.t)(a:Syntax.t) : EvalResult =
        match a with
        | Unit -> ok Unit
        | Bool(_) | Int(_) | Float(_) -> ok a
        | LetIn(Decl(id,_ty), value, Some body) -> eval (e |> Map.add id value) body
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
        | BinOp(op, (lp,l), (rp,r)) ->
            let oid = (Identifier.Id <| sprintf "(%s)" op)
            eval e (App(Var oid, [l; r]))
        | LetIn(_, _, _) -> failwith "Not implemented yet"
        | If((_posConf, eif), (_posThen, ethen), (_posElse, eelse)) ->
            trial {
                let! rif = eval e eif
                return! if rif = Bool true then eval e ethen else eval e eelse
            }
        | Seq(_) -> failwith "Not implemented yet"
        //| _ -> failwith "Not implemented yet"