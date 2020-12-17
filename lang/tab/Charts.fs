module Charts

open System
open TabTypes



(* Evaluate and create text to print for barre section of a chart
1) b is the barre itself
2) x is the x coord
3) y is the y coord
*)
let evalBarre (b: barre) (x: float) (y: float) : String option =

   match b with
   | Barre(fret, startString, endString) ->

      let length = endString - startString

      match (startString, endString) with
      | (s,e) when s > 6 || s < 1 || e > 6 || e < 1 || s > e ->
         printfn "Error in the barre of a chart! The start string and end string must both be between 1 and 6, and the start string cannot be greater than the end string."
         printfn "Here is the problem barre: %A" b
         None

      | (s,e) ->
         Some(" 72 610 1 2 6 drawBarre ")


   | EmptyBarre -> Some(" ")




(* Parse a single chart
1) charts is the list of Chords
2) text is the string to return
3) optionsR has all the options info
4) x is the xCoord
5) y is the yCoord
RETURNS a string to be printed to postscript, or none
*)
let evalChart (chart: TabExpr) (text: String) (optionsR: optionsRecord) (x: float) (y: float) : String option =

   match chart with
   | Chart(barre, spots) ->

      match evalBarre barre x y with
      | Some(barreText) ->

         Some(" 72 610 (6) (F major) chart
         72 610 2 3 drawSpot
         72 610 1 (O) drawOX
         72 610 2 (X) drawOX " + barreText)

      | None -> None


   | _ ->
      printfn "Something very wrong! evalChart given a TabOption or Music instead of a Chart!"
      printfn "Here is the problematic TabExpr: %A" chart
      None




(* Driver for evaluating charts
1) charts is the list of Chords
2) text is the string to return
3) optionsR has all the options info
4) x is the xCoord
5) y is the yCoord
6) count is the number of charts that have already been printed
7) page is what page it is currently printing on
RETURNS a string to be printed to postscript, or none
*)
let rec evalCharts (charts: TabExpr list) (text: String) (optionsR: optionsRecord) (x: float) (y: float) (count: int) (page: int) : String option =

   match charts with
   | [] -> Some(text)
   | head::tail ->


      // call evalChart, then figure out variables for the next recursive call
      match evalChart head text optionsR x y with
      | Some(newText) ->

         Some(newText)
      | None -> None


















//
