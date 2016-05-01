namespace MinFSharp

module Parser =
    open Chessie.ErrorHandling
    open FParsec
    open FParsec.CharParsers
    open FParsecTrace
    open MinFSharp
    open MinFSharp.Syntax

    type ParsingError = string

    type ParsingResult<'U> = Result<Syntax.t<'U>, ParsingError>

    let keywords = Set([ "="; "let"; "in"; "if"; "then"; "else" ])
    let ws = many (anyOf " \t")// spaces
    let nws = spaces
    let pstringws s = skipString s <!> ("pstringws '" + s + "'") .>> ws
    let str = pstringws
    let strn s = pstring s .>> nws

    let parse (s : string) : ParsingResult<'U> =
        let pInt = pint32 |>> Syntax.Int <!> "pInt"
        let pBool = stringReturn "true" (Syntax.Bool true) <|> stringReturn "false" (Syntax.Bool false) <!> "pBool"
        let pId =
            many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c)
            >>= (fun x ->
            if keywords.Contains x then fail "KEYWORD"
            else preturn <| Identifier.Id x .>> ws)
            <!> "pId"
        
        let pExp, pExpImpl = createParserForwardedToRef()

        let pParenExp = between (skipChar '(') (skipChar ')') pExp

        let pDecVal = pId .>> str "=" .>>. pExp |>> (fun (id, exp) -> ((id, Type.Var None), exp))
        
        let pFunArgs = many1 pId |>> List.map (fun x -> (x, Type.Var None))
        let pDecFun = tuple4 pId pFunArgs (str "=") pExp |>> (fun (id, args, _, body) -> ((id, Type.Var None), Syntax.FunDef(args, FBody.Body body)))

        let pDec = attempt pDecFun <|> pDecVal
        let pDeclist = pDec
        let pLet = (str "let") >>. pDeclist .>>. (opt ((strn "in") >>. pExp))
                   |>> (fun ((dVar, dVal), exp) -> match exp with | None -> Syntax.Let(dVar, dVal) | Some exp -> Syntax.LetIn(dVar, dVal, exp))

        let pSimpleExp = choice [
                                  pInt
                                  pBool
                                  attempt (pId |>> Syntax.Var)
                                  attempt pParenExp
                                  pLet
                                  ] .>> ws <!> "pSimpleExp"
        let pAppExps = many1 pSimpleExp |>> (fun l -> if l.Length = 1 then l.Head else Syntax.App(l.Head, l.Tail)) <!> "pAppExps"
        let pOpExp, pOpExpImpl = createParserForwardedToRef()
        
//        let isSymbolicOperatorChar = isAnyOf "!%&*+-./<=>@^|~?"
//        let remainingOpChars_ws = manySatisfy isSymbolicOperatorChar .>> ws
//        let opp = new OperatorPrecedenceParser<Syntax.t<'U>, string, UserState>()
//        let addSymbolicInfixOperators prefix precedence associativity =
//            let op = InfixOperator(prefix, remainingOpChars_ws,
//                                   precedence, associativity, (),
//                                   fun remOpChars expr1 expr2 ->
//                                       BinOp(prefix + remOpChars, expr1, expr2))
//            opp.AddOperator(op)
//        "!%&*+-./<=>@^|~?" |> Seq.iter (fun c -> addSymbolicInfixOperators (c.ToString()) 10 Associativity.Left)
//        opp.TermParser <- attempt pAppExps
//        let pBinOp = opp.ExpressionParser <!> "pBinOp"
        let pBinOp = attempt ( many1 (anyOf "!%&*+-./<=>@^|~?") ) .>> ws |>> (List.toArray >> System.String)

        let pBinOpApp = attempt (tuple3 pAppExps pBinOp pOpExp) |>> (fun (l,o,r) -> Syntax.BinOp(o, l, r))
        pOpExpImpl := choice [pBinOpApp; pAppExps] .>> nws
                      <!> "pOpExp"

        let pIf =
            str "if" >>. pOpExp .>> strn "then" .>>. pOpExp .>> strn "else" .>>. pOpExp
            |>> (fun ((eIf, eThen), eElse) -> Syntax.If(eIf, eThen, eElse))
            <!> "pIf"
    
        pExpImpl := pIf <|> pOpExp
        let p = pExp
        let r =
            FParsec.CharParsers.runParserOnString p ({ Debug =
                                                              { Message = ""
                                                                Indent = 0 } }) "" s
        match r with
        | FParsec.CharParsers.Success(r, _u, _p) -> ok r
        | Failure(e, _err, _pos) -> Chessie.ErrorHandling.Trial.fail e
