namespace MinFSharp

module Parser =
    open Chessie.ErrorHandling
    open FParsec
    open FParsec.CharParsers
    open FParsecTrace
    open MinFSharp
    open MinFSharp.Syntax

    type ParsingError = string * UserState

    type ParsingResult<'U> = Result<Syntax.t<'U>, ParsingError>

    let keywords = Set([ "="; "let"; "in"; "if"; "then"; "else" ])
    let ws = many (anyOf " \t") <!> "ws"// spaces
    let ws1 = many1 (anyOf " \t") <!> "ws1"// spaces
    let nws = spaces
    let pstringws s = skipString s <!> ("pstringws '" + s + "'") .>> ws
    let str = pstringws
    let strn s = pstring s .>> nws

    let parseU (f:UserState -> Unit) (s : string) : ParsingResult<'U> =
        let pInt = pint32 |>> Syntax.Int <!> "pInt"
        let pBool = stringReturn "true" (Syntax.Bool true) <|> stringReturn "false" (Syntax.Bool false) <!> "pBool"
        let pId =
            many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c)
            >>= (fun x ->
            if keywords.Contains x then fail "KEYWORD"
            else preturn <| Identifier.Id x)
            <!> "pId"

        let pExp, pExpImpl = createParserForwardedToRef()
        let pBlockExp, pBlockExpImpl = createParserForwardedToRef()

        let pParenExp = between (skipChar '(') (skipChar ')') pExp

        let pTypeAnn= choice [
                               stringReturn "int" Type.Int
                               stringReturn "bool" Type.Bool
                               stringReturn "float" Type.Float
                               stringReturn "unit" Type.Unit
                             ]
                        .>> ws .>>. (opt <| str "array")
                        |>> (fun (t, arr) -> if Option.isNone arr then t else Type.Array t)
        let pTypeTuple = sepBy1 pTypeAnn (str "*") |>> (fun l -> if l.Length = 1 then l.Head else Type.Tuple l)
        let pTypeFun = sepBy pTypeTuple (str "->") |>> (fun l -> if l.Length = 1 then l.Head else let n = List.length l in Type.Fun(List.take (n-1) l, List.item (n-1) l))
        let pOptTypeAnn = opt (str ":" >>. pTypeFun .>> ws)
                       |>> function
                           | None -> Type.Var None
                           | Some (t) -> t
                       <!> "pOptTypeAnn"
        let pDecVal = pId .>> ws .>>. pOptTypeAnn .>> str "=" .>>. pExp |>> (fun ((id, t), exp) -> ((id, t), exp))

        let pFunArgs = many1 (pId .>>? ws1) |>> List.map (fun x -> (x, Type.Var None))
        let pDecFun = tuple4 (pId .>>? ws1) pFunArgs (str "=") (pExp .>> ws)
                      |>> (fun (id, args, _, body) -> ((id, Type.Var None), Syntax.FunDef(args, FBody.Body body, Type.Var None)))
                      <!> "pDecFun"

        let pDec = attempt pDecFun <|> pDecVal
        let pDeclist = pDec
        let pLet = (str "let") >>. pDeclist .>>. (opt ((strn "in") >>. pExp))
                   |>> (fun ((dVar, dVal), exp) -> Syntax.LetIn(dVar, dVal, exp))

        let pSimpleExp = choice [
                                  stringReturn "()" Syntax.Unit
                                  pInt
                                  pBool
                                  attempt (pId |>> Syntax.Var)
                                  attempt pParenExp
                                  pLet
                                  ] <!> "pSimpleExp"
        let pAppExps = pSimpleExp .>>. opt (many (ws1 >>? pSimpleExp)) .>> ws
                       |>> (fun (h,l) -> match l with
                                         | None | Some [] -> h
                                         | Some l -> Syntax.App(h, l))
                       <!> "pAppExps"
        let pOpExp, pOpExpImpl = createParserForwardedToRef()

        let pBinOp = attempt ( many1 (anyOf "!%&*+-./<=>@^|~?") ) .>> ws |>> (List.toArray >> System.String) <!> "pBinOp"

        let pBinOpApp = attempt (tuple3 pAppExps pBinOp pOpExp) |>> (fun (l,o,r) -> Syntax.BinOp(o, l, r))
        pOpExpImpl := choice [pBinOpApp; pAppExps] .>> ws
                      <!> "pOpExp"

        let pIf =
            str "if" >>. pOpExp .>> strn "then" .>>. pOpExp .>> nws .>> strn "else" .>> nws .>>. pOpExp
            |>> (fun ((eIf, eThen), eElse) -> Syntax.If(eIf, eThen, eElse))
            <!> "pIf"

        pBlockExpImpl := pIf <|> pOpExp <!> "pBlockExp"
        pExpImpl := sepBy1 pBlockExp (strn ";" |>> ignore <|> (anyOf "\n" .>> ws |>> ignore <!> "p \\n")) |>> (fun l -> if List.length l = 1 then l.Head else Syntax.Seq l) <!> "pExp"
        let p = pExp
        let r =
            FParsec.CharParsers.runParserOnString p ({ Debug =
                                                              { Message = ""
                                                                Indent = 0 } }) "" s
        match r with
        | FParsec.CharParsers.Success(r, u, _p) -> f u; ok r
        | Failure(e, (err), u) -> f u; Chessie.ErrorHandling.Trial.fail (e,u)
    let parse s : ParsingResult<'U> = parseU ignore s
