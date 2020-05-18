namespace testing

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjectParser
open ProjectInterpreter

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.IsTrue(true);
