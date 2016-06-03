namespace MinFSharp

module Typing =
    open Chessie.ErrorHandling
    open Chessie.ErrorHandling.Trial

    type TypeMismatch = {expected:Type.t; actual: Type.t}

    type TypingErrorType =
        | UnknownError
        | UnknownSymbol of Identifier.t
        | TypeMismatch of TypeMismatch
    type TypingError = Syntax.Pos * TypingErrorType

    let typeMismatchAt (exp:Type.t) (act:Type.t) (p:Syntax.Pos) =
            p, TypingErrorType.TypeMismatch {expected = exp; actual = act}

    let typeMismatch (exp:Type.t) (act:Type.t) =
            Syntax.Pos.zero, TypingErrorType.TypeMismatch {expected = exp; actual = act}

    type TypingResult = Result<Type.t, TypingError>

    type TypedAstResult = Result<Type.t, TypingError>

    let rec typed (env:Env.Type ref) x : TypedAstResult =
        match x with
        | Syntax.Lit Syntax.Unit -> ok Type.Unit
        | Syntax.Lit(Syntax.Bool(_)) -> ok Type.Bool
        | Syntax.Lit(Syntax.Int(_)) -> ok Type.Int
        | Syntax.Lit(Syntax.Float(_)) -> ok Type.Float
        | Syntax.BinOp(op, a, b) -> typedBinOp env op a b

        | Syntax.LetIn(Syntax.Decl(vid, vty), (_pva,va), insOpt) ->
            trial {
                let! tyVa = typed env va
//                match vty with
//                | Type.Var v -> ()
//                | _ -> return! fail <| typeMismatch tyVa
                let newEnv = Env.add vid tyVa !env |> ref
                match insOpt with
                | None -> return Type.Unit
                | Some (_,ins) ->
                    let! tyIns = typed newEnv ins
                    match vty with
                    | Type.Var(vt) when !vt = None ->
                        vt := Some(Type.Link tyVa)
                        return tyIns
                    | t when t = tyVa -> return tyIns
                    | _ -> return! fail <| typeMismatch tyVa vty
            }
        | Syntax.If((posCond,cond), (_posThen, ethen), (posElse, eelse)) ->
            trial {
                let! tcond = typed env cond
                if tcond <> Type.Bool then return! fail <| typeMismatchAt Type.Bool tcond posCond
                let! tthen = typed env ethen
                let! telse = typed env eelse
                if tthen <> telse then return! fail <| typeMismatchAt tthen telse posElse
                return tthen
            }
        | Syntax.Var(v) ->
            match (!env).tryFind v with
            | None -> fail (Syntax.Pos.zero, UnknownSymbol v)
            | Some(tyv) ->
                match tyv with
                | Type.Var V when !V = None ->
                    let nextTypeVar = Env.nextPolyType env
                    V := Some nextTypeVar
                    env := !env |> Env.add v (Type.Var V); ok (Type.Var V)// typed env vd
                | _ -> ok (tyv)// typed env vd
        | Syntax.FunDef(args, Syntax.FBody.Ext _ext, ret) ->
            ok (Type.arrow((args |> List.map Syntax.declType) @ [ret]))
        | Syntax.FunDef(args, Syntax.FBody.Body body, ret) ->
            trial {
                let newEnv = args |> List.fold(fun e (Syntax.Decl(argId,argTy)) -> Env.add argId argTy e) !env |> ref
                let! tret = typed newEnv body
                let args = args |> List.map (fun (Syntax.Decl(argId,_argTy)) -> Syntax.Decl(argId, Env.find argId !newEnv))
                return Type.arrow((args |> List.map Syntax.declType) @ [tret])
            }
        | Syntax.App(func, args) ->
            trial {
                let! tfunc = typed env func
                return! typedApp env tfunc args
            }
        | Syntax.Seq(s) ->
            trial {
                let! ts = s |> List.map (snd >> (typed env)) |> Trial.collect
                let tss = List.zip (s |> List.map fst) ts
                return (if tss.Length = 0 then Type.Unit else List.last ts)
            }

    and typedApp env tyFunc args =
        let rec typeArrow tf args =
            trial {
                match tf, args with
                | Type.Fun(_, _), [] -> return tf
//FIXME
//                | Type.Fun(Type.Poly p, y), h::t ->
//                    let inst = y |> instantiate_at p h
//                    return! typeArrow inst t
                | Type.Fun(x, y), h::t when x = h -> return! typeArrow y t
                | Type.Fun(x, _), h::_ when x <> h -> return! fail <| typeMismatch x h
                | t, [] -> return t
                | _ -> return! fail (Syntax.Pos.zero, UnknownError)
            }
        trial {
            let! targs = args |> List.map (typed env) |> Trial.collect
            return! typeArrow tyFunc targs
        }

    and unify tExp tAct =
        match tExp,tAct with
        | a,b when a = b -> ok tExp
//        | Type.Poly p, x -> ok x
        | _,_ -> fail (typeMismatch tExp tAct)

    and typedBinOp (env) op (ap, a) (bp, b) =
        let opId = Syntax.opName op |> Identifier.Id
        trial {
            match (!env).tryFind opId with
            | None -> return! fail (Syntax.Pos.zero, UnknownSymbol opId)
            | Some tyop -> return! typedApp env tyop [a;b]
        }

    and typed_deref_rec x =
        let rec do_deref_type t =
            match t with
            | Type.Var v -> match !v with
                            | Some(Type.Link vv) -> do_deref_type vv
                            | _ -> t
            | _ -> t
        let f = typed_deref_rec
        let fp (pos,x) = (pos, f x)
        match x with
        | Syntax.Lit _ | Syntax.Var _ -> x
        | Syntax.BinOp(op, (posa, a), (posb, b)) -> Syntax.BinOp(op, (posa, f a), (posb, f b))
        | Syntax.LetIn(Syntax.Decl(id,ty), value, scope) ->Syntax.LetIn(Syntax.Decl(id, do_deref_type ty), value, scope)
        | Syntax.If(ei, et, ee) -> Syntax.If(fp ei, fp et, fp ee)
        | Syntax.FunDef(args, body, ty) ->
            let args = args |> List.map (fun (Syntax.Decl(id,ty)) -> Syntax.Decl(id, do_deref_type ty))
            let body = match body with
                       | Syntax.Ext e -> body
                       | Syntax.Body b -> Syntax.Body (f b)
            Syntax.FunDef(args, body, do_deref_type ty)
        | Syntax.App(fu, args) -> Syntax.App(f fu, args |> List.map f)
        | Syntax.Seq(s) -> Syntax.Seq(s |> List.map fp)

    and typed_deref t x =
        let ast = typed_deref_rec x
        match t with
        | Type.Unit -> x
        | _ -> Syntax.Internal(Syntax.Ignore x)

    and instantiate_at i t poly =
        match poly with
        | Type.Var v -> match !v with
                        | Some(Type.Link tv) -> instantiate_at i t tv
                        | Some(Type.Poly a) when a = i -> t
                        | _ -> poly
        | Type.Fun(a,b) -> Type.Fun(instantiate_at i t a, instantiate_at i t b)
        | _ -> poly

    let rec instantiate t args =
        args |> List.indexed |> List.fold(fun t (idx,tya) ->
            match Seq.tryItem idx args with
            | Some(Some i) -> instantiate_at (uint32 idx) i t
            | _ -> t) t
//        match t with
//        | Type.Poly(a) -> match Seq.tryItem (int32 a) args with
//                          | Some(Some i) -> i
//                          | _ -> t
//        | Type.Var v -> match !v with
//                        | None -> t
//                        | Some tv -> tv
//        | Type.Fun(a,b) -> Type.Fun(instantiate a args, instantiate b args)
//        | _ -> t
