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

    type TypedAstResult = Result<Syntax.t * Type.t, TypingError>

    let rec typed (env:Env.t) x : TypedAstResult =
        match x with
        | Syntax.Unit -> ok (x, Type.Unit)
        | Syntax.Bool(_) -> ok (x, Type.Bool)
        | Syntax.Int(_) -> ok (x, Type.Int)
        | Syntax.Float(_) -> ok (x, Type.Float)
        | Syntax.BinOp(op, a, b) -> typedBinOp env op a b
            
        | Syntax.LetIn(Syntax.Decl(vid, vty), va, insOpt) ->
            trial {
                let! va, tyVa = typed env va
//                match vty with
//                | Type.Var (Some x)
                let newEnv = env |> Map.add vid va
                match insOpt with
                | None -> return Syntax.LetIn(Syntax.Decl(vid, tyVa), va, None), Type.Unit
                | Some ins ->
                    let! ins, tyIns = typed newEnv ins
                    match vty with
                    | Type.Var None -> return Syntax.LetIn(Syntax.Decl(vid, tyVa), va, Some ins), tyIns
                    | t when t = tyVa -> return Syntax.LetIn(Syntax.Decl(vid, tyVa), va, Some ins), tyIns
                    | _ -> return! fail <| typeMismatch tyVa vty
            }
        | Syntax.If((posCond,cond), (posThen, ethen), (posElse, eelse)) ->
            trial {
                let! cond, tcond = typed env cond
                if tcond <> Type.Bool then return! fail <| typeMismatch Type.Bool tcond
                let! ethen, tthen = typed env ethen
                let! eelse, telse = typed env eelse
                if tthen <> telse then return! fail <| typeMismatch tthen telse
                return Syntax.If((posCond, cond), (posThen, ethen), (posElse, eelse)), tthen
            }
        | Syntax.Var(v) ->
            match Map.tryFind v env with
            | None -> fail (Syntax.Pos.zero, UnknownSymbol v)
            | Some vd -> typed env vd
        | Syntax.FunDef(args, Syntax.FBody.Ext body, ret) ->
            ok (x, Type.arrow((args |> List.map snd) @ [ret]))
        | Syntax.FunDef(args, Syntax.FBody.Body body, ret) ->
            trial {
//                let newEnv = args |> List.fold(fun e (argId,argTy) -> e |> Map.add argId (Syntax.Var())) env
                
                let! tr, tyr = typed env body
                return (x, Type.arrow((args |> List.map snd) @ [ret]))
            }
        | Syntax.App(func, args) ->
            let rec typeArrow f tf args =
                trial {
                    match tf, args with
                    | Type.Fun(_, _), [] -> return f,tf
                    | Type.Fun(x, y), h::t when x = h -> return! typeArrow f y t
                    | Type.Fun(x, _), h::_ when x <> h -> return! fail <| typeMismatch x h
                    | t, [] -> return f, t
                    | _ -> return! fail (Syntax.Pos.zero, UnknownError)
                }
            trial {
                let! func, tfunc = typed env func
                let! typedArgs = args |> List.map (typed env) |> Trial.collect
                let args, targs = List.unzip typedArgs
                printfn "%A" targs
                return! typeArrow func tfunc targs
            }
        | Syntax.Seq(s) ->
            trial {
                let! ts = s |> List.map (snd >> (typed env)) |> Trial.collect
                let tss = List.zip (s |> List.map fst) (ts |> List.map fst)
                return Syntax.Seq tss, (if tss.Length = 0 then Type.Unit else List.last ts |> snd)
            }

    and typedBinOp (env) op (ap, a) (bp, b) =
        trial {
            let! a, tya = typed env a
            let! b, tyb = typed env b
            let opId = Syntax.opName op |> Identifier.Id
            match Map.tryFind opId env with
            | None -> return! fail (Syntax.Pos.zero, UnknownSymbol opId)
            | Some o ->
                let! _top, tyop = typed env o
                match tyop with
                | Type.Fun(atya, Type.Fun(atyb, tret)) when atya = tya && atyb = tyb ->
                    return Syntax.BinOp(op, (ap, a), (bp, b)), tret
                | Type.Fun(atya, Type.Fun(atyb, tret)) when atya <> tya || atyb <> tyb ->
                    if atya <> tya then
                        return! fail (typeMismatch atya tya)
                    else
                        return! fail (typeMismatch atyb tyb)
                | Type.Fun(_args,_ret) -> return! fail (typeMismatchAt (Type.Var None) (tyop) ap)
                | _ -> return! fail (typeMismatch (Type.arrow [tya; tyb; Type.Var None]) tyop)
        }

//    let rec typing env a : TypingResult =
//        match a with
//        | Syntax.Unit -> ok Type.Unit
//        | Syntax.Bool(_) -> ok Type.Bool
//        | Syntax.Int(_) -> ok Type.Int
//        | Syntax.Float(_) -> ok Type.Float
//        | Syntax.LetIn((_vid,_), vval,None) ->  typing env vval
//        | Syntax.LetIn((vid,_), vval, Some e) ->
//            trial {
//                let! tVal = typing env vval
//                let nEnv = env |> Map.add vid tVal
//                return! typing nEnv e
//            }
//        | Syntax.Var(vid) ->
//            Map.tryFind vid env |> failIfNone (Syntax.Pos.zero, UnknownSymbol(vid))
//        | Syntax.FunDef(args, Syntax.FBody.Ext body, _ret) -> fail (Syntax.Pos.zero,UnknownError)
//        | Syntax.FunDef(args, Syntax.FBody.Body body, _ret) ->
//            trial {
//                let targs = args |> List.map (snd)
//
//                let! tvody = typing env body
//                return Type.arrowr targs tvody
//            }
//        | Syntax.App(_, _) -> failwith "Not implemented yet"
//        | Syntax.Seq l -> l |> List.map (typing env) |> Trial.collect |> Trial.bind (Seq.last >> ok)