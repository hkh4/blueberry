namespace testing

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjectParser
open ProjectInterpreter

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.ParseOptions() =
      let input =
         "-type tab
-key c
"
      let expected =
         ([ScoreOption ("type"," tab"); ScoreOption ("key"," c")], [])
      let result = parse input
      match result with
      | Some(ast) -> Assert.AreEqual(expected,ast)
      | None -> Assert.IsTrue false
