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

    let keywords = Set([ "let"; "in"; "if"; "then"; "else" ])

    let parse (s : string) : ParsingResult<'U> =
        let ws = spaces
        let pstringws s = pstring s <!> ("pstringws " + s) .>> ws
        let pInt = pint32 |>> Syntax.Int <!> "pInt"
        let pBool = stringReturn "true" (Syntax.Bool true) <|> stringReturn "false" (Syntax.Bool false) <!> "pBool"

        //        let pId = letter .>>. many (letter <|> digit) |>> (fun (f,r)
        //->System.String.Concat(Array.ofList(f :: r)) |> (Identifier.Id))
        let pId =
            many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c)
            >>= (fun x ->
            if keywords.Contains x then fail "KEYWORD"
            else preturn <| Identifier.Id x)
            <!> "pId"
        let pSimpleExp = pInt <|> pBool <|> attempt (pId |>> Syntax.Var) .>> ws <!> "pSimpleExp"
        let pExp, pExpImpl = createParserForwardedToRef()
        let pExpNoApp, pExpNoAppImpl = createParserForwardedToRef()
        let pDecl = pId .>> ws .>> pstringws "=" .>>. pExp .>> ws <!> "pDecl"

        let pFunArg = pId
        let pFunDecl =
            pId .>> ws .>>. sepBy1 pFunArg ws |>> (fun (fid, fargs) -> fid,fargs |> List.map (fun aid -> (aid, Type.Var None)) )
        let pLetFun =
            pstringws "let" >>. pFunDecl .>> pstringws "=" .>>. pExp
            |>> (fun ((fid, fargs), fbody) -> Syntax.Let((fid, Type.Var None), Syntax.FunDef(fargs, FBody.Body fbody), Unit))
            <!> "pLetFun"
        let pLet =
//            pstringws "let" >>. pDecl .>> pstringws "in" .>>. pExp
            pstringws "let" >>. pDecl .>> pstringws "in" .>>. pExp
            |>> (fun ((id, va), a) -> Syntax.Let((id, Type.Int), va, a))
            <!> "pLet"

        let pIf =
            pstringws "if" >>. pExp .>> pstringws "then" .>>. pExp .>> pstringws "else" .>>. pExp
            |>> (fun ((eIf, eThen), eElse) -> Syntax.If(eIf, eThen, eElse))
            <!> "pIf"

        let pParExp = between (pchar '(') (pchar ')') pExp .>> ws <!> "pParExp"

        let pApp = pSimpleExp .>>. (many1 pExpNoApp) |>> (fun (a,b) -> Syntax.App(a,b)) <!> "pApp"
//        let pApp = pSimpleExp .>>. (pExp) |>> (fun (a,b) -> Syntax.App(a,[b])) <!> "pApp"

        pExpNoAppImpl := attempt pIf <|> attempt pLet <|> attempt pParExp <|> pSimpleExp <!> "pExp"
        pExpImpl := choice [ attempt pApp
                             attempt pIf
                             attempt pLetFun
                             attempt pLet
                             attempt pParExp
                             pSimpleExp
                    ]<!> "pExp"

        let r =
            FParsec.CharParsers.runParserOnString pExp ({ Debug =
                                                              { Message = ""
                                                                Indent = 0 } }) "" s
        match r with
        | FParsec.CharParsers.Success(r, _u, _p) -> ok r
        | Failure(e, _err, _pos) -> Chessie.ErrorHandling.Trial.fail e
