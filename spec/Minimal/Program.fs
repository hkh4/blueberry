// Learn more about F# at http://fsharp.org
 
open System
open System.IO
open Parser
open CS334
open Eval

[<EntryPoint>]
let main argv =
   if (Array.length argv = 0) then
      1
   else
      let fileName = argv.[0]
      let fileText = System.IO.File.ReadAllText fileName
      printfn "%A" fileText
      match parse fileText with
      | Some ast ->
         match (eval ast) with
         | Some(_) -> 0
         | None -> 1
      | None ->
         printfn "Error!"
         0
