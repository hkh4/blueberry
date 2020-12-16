module TabInterpreter

open System
open System.IO
open TabTypes
open TabParser
open TabOptions
open Charts


//******************* DRIVER
(* Main eval method
1) parsed is the result of the parser
*)
let tabEval parsed =

   // deconstruct
   let (opts, charts) = parsed

   printfn "%A" charts

   // parse the options
   match evalOption opts defaultOptionsRecord with
   | Some(optionsR) ->

      match evalCharts charts "" with
      | Some(chartText) ->
         printfn "%A" chartText
         0
      | None -> 1
   | None -> 1

































//























//
