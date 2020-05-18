// Learn more about F# at http://fsharp.org
 
open System
open System.IO
open Parser
open ProjectParser
open ProjectInterpreter

[<EntryPoint>]
let main argv =
   //reads in from a file
   if (Array.length argv = 0) then
      printfn "Correct usage: dotnet run <file>"
      1
   else
      let fileName = argv.[0]
      let fileText = System.IO.File.ReadAllText fileName
      match parse fileText with
      | Some ast ->
         match (eval ast) with
         | Some(_) -> 0
         | None -> 1
      | None ->
         printfn "Error!"
         0
