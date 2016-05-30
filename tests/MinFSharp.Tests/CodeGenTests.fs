namespace MinFSharp.Tests

module CodeGenTests =

    open MinFSharp
    open NUnit.Framework

    [<Test>]
    let ``gen asm``() =
        Codegen.gen (Syntax.Lit(Syntax.Int 42)) "test.exe"
