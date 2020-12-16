module Charts

open System
open TabTypes


(* Parse a single chart
1) charts is the list of Chords
2) text is the string to return
RETURNS a string to be printed to postscript, or none
*)
let evalChart (chart: TabExpr) (text: String) : String option =

   match chart with
   | Chart(barre, spots) ->
      None

   | _ ->
      printfn "Something very wrong! evalChart given a TabOption or Music instead of a Chart!"
      printfn "Here is the problematic TabExpr: %A" chart
      None




(* Driver for evaluating charts
1) charts is the list of Chords
2) text is the string to return
RETURNS a string to be printed to postscript, or none
*)
let rec evalCharts (charts: TabExpr list) (text: String) : String option =

   match charts with
   | [] -> Some(text)
   | head::tail ->

      match evalChart head text with
      | Some(newText) -> evalCharts tail newText
      | None -> None


















//
