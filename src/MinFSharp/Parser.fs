namespace MinFSharp

module Parser =

    open FParsec
    open FParsec.CharParsers
    open Chessie.ErrorHandling
    open FParsecTrace
    open MinFSharp
    open MinFSharp.Syntax

    type ParsingError = string
    type ParsingResult = Result<Syntax.t,ParsingError>
    let parse (s:string) : ParsingResult =
        let ws = skipAnyOf [' '; '\t']
        let pInt = pint32 |>> Syntax.Int <!> "pInt"
        let pId = letter .>>. many (letter <|> digit) |>> (fun (f,r) ->System.String.Concat(Array.ofList(f :: r)) |> (Identifier.Id >> Syntax.Var))
        let pSimpleExp = pInt <|> pId <!> "pSimpleExp"

        let pExp,pExpImpl = createParserForwardedToRef()
        let pApp = pSimpleExp .>> ws .>>. (sepBy1 pSimpleExp ws) |>> Syntax.App <!> "pApp"
        pExpImpl := attempt pApp <|> pSimpleExp <!> "pExp"

        let r = FParsec.CharParsers.runParserOnString pExp ({ Debug = { Message = ""; Indent = 0 } }) "" s
        match r with
        | FParsec.CharParsers.Success(r,_u,_p) -> ok r
        | Failure(e,_err,_pos) -> fail e


