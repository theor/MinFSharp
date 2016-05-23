namespace MinFSharp

module Type =
    type t =
    | Unit
    | Bool
    | Int
    | Float
    | Fun of t * t
    | Tuple of t list
    | Array of t
    | Poly of uint32
    | Var of t option ref
    with
        override x.ToString() =
            let tstr x = x.ToString()
            match x with
            | Fun(l, r) -> [l;r] |> List.map tstr |> String.concat " -> "
            | Tuple(l) -> l |> List.map tstr |> String.concat " * "
            | Array(t) -> sprintf "%O array" t
            | Var(t) -> sprintf "'%O" t
            | _ -> sprintf "%A" x
    let rec arrow l =
        match l with
        | [] -> failwith "ARROW"
        | t :: [] -> t
        | t1 :: t2 -> Fun(t1, arrow t2)
    let arrowr l r = arrow(l @ [r])
    let var s = s |> Some |> ref |> Var
    let genType = Var(ref None)