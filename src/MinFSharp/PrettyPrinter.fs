namespace MinFSharp

module PrettyPrinter =
    open MinFSharp.Syntax
    open MinFSharp.Identifier

    type Options = {indent:int}
    let defaultOptions = {indent=2}
    let ind i = String.replicate (i*2) " "
    let rec print_type (t:Type.t) =
        let collect l = if List.forall Option.isSome l then List.map Option.get l |> Some else None
        match t with
        | Type.Array(t) -> (print_type t) |> Option.map (sprintf "%s array")
        | Type.Fun(l, r) ->
            match print_type l, print_type r with
            | Some l, Some r -> sprintf "%s -> %s" l r |> Some
            | _ -> None
        | Type.Tuple(l) -> l |> List.map print_type |> collect |> Option.map (String.concat " * ")
        | Type.Var x when !x = None -> None
        | _ -> Some (t.ToString().ToLowerInvariant())
    let print_vardecl (Decl(Id id,t)) =
        match print_type t with
        | None -> id
        | Some t -> sprintf "%s : %s" id t
    let rec print_rec (opt:Options) indent (ast:Syntax.t) =
        let p = print_rec opt
        let pi = p indent
        let pinc = p (indent+1)
        match ast with
        | Lit(Unit) -> "()"
        | Lit(Bool b) -> if b then "true" else "false"
        | Lit(Int i) -> sprintf "%i" i
        | Lit(Float f) -> sprintf "%f" f
        | BinOp(op, (_pl, l), (_pr, r)) -> sprintf "(%s %s %s)" (pi l) op (pi r)
        | LetIn(id, (_p,value), None) -> sprintf "let %s = %s" (print_vardecl id) (pi value)
        | LetIn(id, (_p,value), Some (_,scope)) ->
            sprintf "let %s = %s in\n%s" (print_vardecl id) (pi value) (pinc scope)
        | If((_pc, c), (_pt, t), (_pe, e)) ->
            let i = ind (indent+1)
            sprintf "if %s then\n%s%s\nelse\n%s%s" (pi c) i (pinc t) i (pinc e)
        | Var(Id id) -> id
        | FunDef(args, Body body, ty) ->
            let sArgs = (args |> List.map print_vardecl |> String.concat " ")
            let idt = ind (indent+1)
            match print_type ty with
            | Some t -> sprintf "%s : %s =\n%s%s" sArgs t idt (pinc body)
            | None -> sprintf "%s =\n%s%s" sArgs idt (pinc body)
        | App((_,f), args) -> String.concat " " (f :: args |> List.map pi)
        | Seq(s) -> String.concat "\n" (s |> List.map (snd >> pi))
        | Internal(Ignore x) -> sprintf "ignore (%s)" (p 0 x)

    let print (opt:Options) (ast:Syntax.t) = print_rec opt 0 ast