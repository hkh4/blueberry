// Learn more about F# at http://fsharp.org

open System
open System.IO
open FParsec
open ProjectParser
open ProjectInterpreter

[<EntryPoint>]
let main argv =
   //reads in from a file
   if (Array.length argv <> 2) then
      printfn "Correct usage: dotnet run <file> <outFile>"
      1
   else
      let fileName = argv.[0]
      let outFile = argv.[1]
      let fileText = System.IO.File.ReadAllText fileName
      match run grammar fileText with
      | Success(result, _, _) ->
         let (optionsList, measuresList) = result
         printfn "%A" result
         match eval optionsList measuresList outFile with
         | Some(text,pages) -> 0
         | None -> 1
      | Failure(errorMsg, _, _) ->
         printfn "Failure: %s" errorMsg
         1
