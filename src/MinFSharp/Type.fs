namespace MinFSharp

module Type =
    [<CustomEquality;CustomComparison>]
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
//        override x.GetHashCode() = System.ValueType.
        override x.Equals(yobj) =
            match yobj with
            | :? t as y ->
                match x,y with
                | Unit,Unit | Bool, Bool | Float, Float | Int, Int -> true
                | Fun(ax, bx), Fun(ay, by) -> ax = ay && bx = by
                | Tuple a, Tuple b -> a = b
                | Array a, Array b -> a = b
                | Poly a, Poly b -> a = b
                | Var vx, Var vy  -> !vx = !vy
                | Var vx, _  -> !vx = Some y
                | _, Var vy -> Some x = !vy
                | _, _ -> false
            | _ -> false
        interface System.IComparable with
            member x.CompareTo(y) = compare x (y :?> t)
    let rec arrow l =
        match l with
        | [] -> failwith "ARROW"
        | t :: [] -> t
        | t1 :: t2 -> Fun(t1, arrow t2)
    let arrowr l r = arrow(l @ [r])
    let var s = s |> Some |> ref |> Var
    let genType() = Var(ref None)