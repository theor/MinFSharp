namespace MinFSharp.Tests

module ParserTests =

    open System
    open MinFSharp
    open MinFSharp.Syntax
    open MinFSharp.Interpreter
    open MinFSharp.Identifier
    open NUnit.Framework
    open FsUnitTyped
    open Chessie.ErrorHandling

    let d name str ast  =
        TestCaseData(str, ast).SetName(name)
    type TCS() =
        static member Data() =
            [|  d "parse single int" "42" (Int 42)
                d "parse app" "f 42 13" (App(Var(Id "f"), [Int 42; Int 13]))
            |]

    let testParseOk (s:string) (a:Syntax.t) =
        match MinFSharp.Parser.parse s with
        | Ok(ast,_) -> ast |> shouldEqual a
        | Bad(e) -> failwith (e.ToString())

    [<TestCaseSource(typeof<TCS>, "Data")>]
    let ``parse int`` (s:string,a:Syntax.t) =
        testParseOk s a