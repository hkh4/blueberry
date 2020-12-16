module TabInterpreter

open System
open System.IO
open TabTypes
open TabParser
open TabOptions


//******************* DRIVER
(* Main eval method
1) parsed is the result of the parser
*)
let tabEval parsed =

   // deconstruct
   let (opts, charts) = parsed

   // parse the options
   match evalOption opts defaultOptionsRecord with
   | Some(optionsR) ->
      printfn "%A" optionsR
      0
   | None ->
      1























//
