namespace MinFSharp

module Parser =
    open Chessie.ErrorHandling
    open FParsec
    open FParsec.CharParsers
    open FParsecTrace
    open MinFSharp
    open MinFSharp.Syntax

    type ParsingError = string * UserState

    type ParsingResult = Result<post, ParsingError>

    let keywords = Set([ "="; "let"; "in"; "if"; "then"; "else" ])
    let ws = many (anyOf " \t") <!> "ws"// spaces
    let ws1 = many1 (anyOf " \t") <!> "ws1"// spaces
    let nws = spaces
    let pstring s = skipString s <!> ("pstringws '" + s + "'")
    let pstringws s = skipString s <!> ("pstringws '" + s + "'") .>> ws
    let str = pstringws
    let strn s = pstring s .>> nws

    let range p = tuple3 getPosition p getPosition |>> (fun (ps, x, pe) -> (Pos.fromRange ps pe),x)

    let parseU (f:UserState -> Unit) (s : string) : ParsingResult =
        let pInt = getPosition .>>. pint32 |>> (fun (p,i) -> (*p,*)Lit(Int i)) <!> "pInt"
        let pBool = stringReturn "true" (Lit <| Bool true) <|> stringReturn "false" (Lit <| Bool false) <!> "pBool"
        let pId =
            many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c)
            >>= (fun x ->
            if keywords.Contains x then fail "KEYWORD"
            else preturn <| Identifier.Id x)
            <!> "pId"

        let (pExp:Parser<post,UserState>), pExpImpl = createParserForwardedToRef()

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
        let pTypeFun = sepBy1 pTypeTuple (str "->")
                       |>> (fun l ->
                            if l.Length = 1 then l.Head else
                            Type.arrow l)
        let pOptTypeAnn = opt (str ":" >>. pTypeFun .>> ws)
                       |>> function
                           | None -> Type.genType()
                           | Some (t) -> t
                       <!> "pOptTypeAnn"
        let pDecVal = pId .>> ws .>>. pOptTypeAnn .>> str "=" .>>. pExp |>> (fun ((id, t), exp) -> ((id, t), exp))

        let pFunArgPar = attempt <| between (str "(") (pstring ")") (pId .>>.? pOptTypeAnn) <!> "pFunArgPar"
        let pFunArgs = many1 (choice [pFunArgPar;pId .>>. preturn (Type.genType())] .>>? ws)
                       |>> List.map (fun (x,t) -> Decl(x, t))
                       <!> "pFunArgs"
        let pDecFun = tuple5 (pId .>>? ws1) pFunArgs pOptTypeAnn (str "=") (pExp .>> ws) |> range
                      |>> (fun (pos, (id, args, ret, _, (_,body))) ->
                             ((id, Type.genType()), (pos,FunDef(args, FBody.Body body, ret))))
                      <!> "pDecFun"

        let pDec = attempt pDecFun <|> pDecVal
        let pDeclist = pDec
        let pLet = (str "let") >>. pDeclist .>>. (opt ((strn "in") >>. range pExp))
                   |>> (fun ((dVar, dVal), exp) -> LetIn(Decl dVar, dVal, Option.map snd exp))

        let pSimpleExp = choice [
                                  stringReturn "()" (Lit Unit)
                                  pInt
                                  pBool
                                  attempt (pId |>> Var)
                                  attempt (pParenExp |>> snd)
                                  pLet
                                  ] <!> "pSimpleExp"
        let pAppExps = pSimpleExp .>>. opt (many (ws1 >>? pSimpleExp)) .>> ws
                       |>> (fun (h,l) -> match l with
                                         | None | Some [] -> h
                                         | Some l -> App(h, l))
                       <!> "pAppExps"
        let pOpExp, pOpExpImpl = createParserForwardedToRef()

        let pBinOp = attempt ( many1 (anyOf "!%&*+-./<=>@^|~?") ) .>> ws |>> (List.toArray >> System.String) <!> "pBinOp"

        let pBinOpApp = attempt (tuple3 (range pAppExps) pBinOp (range pOpExp))
                        |>> (fun (l,o,r) -> BinOp(o, l, r))
        pOpExpImpl := choice [pBinOpApp; pAppExps] .>> ws
                      <!> "pOpExp"

        let pIf =
            str "if" >>. (range pOpExp) .>> strn "then" .>>. (range pOpExp) .>> nws .>> strn "else" .>> nws .>>. (range pOpExp)
            |>> (fun ((eIf, eThen), eElse) -> If(eIf, eThen, eElse))
            <!> "pIf"
        let pBlockExp = pIf <|> pOpExp <!> "pBlockExp"
        pExpImpl := range (sepBy1 (range pBlockExp) (strn ";" |>> ignore <|> (anyOf "\n" .>> ws |>> ignore <!> "p \\n")))
                    |>> (fun (p,l) -> if List.length l = 1 then l.Head else (p,Seq l))
                    <!> "pExp"
        let p = pExp
        let r =
            FParsec.CharParsers.runParserOnString p ({ Debug =
                                                              { Message = ""
                                                                Indent = 0 } }) "" s
        match r with
        | FParsec.CharParsers.Success(r, u, _p) -> f u; ok r
        | Failure(e, (_err), u) -> f u; Chessie.ErrorHandling.Trial.fail (e,u)
    let parse s : ParsingResult = parseU ignore s
