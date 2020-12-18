module Charts

open System
open TabTypes



(* Evaluate a single spot
1) s is the spot
2) x is the x coord
3) y is the y coord
4) baseFret is the value of the smallest fret
RETURNS the string with the text code, and 1 if string 1 fret 1 has a spot
*)
let evalSpot (s: spot) (x: float) (y: float) (baseFret: int) : (String * int) option =

   match s with
   | Spot(fret, guitarString) ->

      let newFret = fret - baseFret + 1

      // if there's a spot on string 1 fret one, need to move the fret number for formatting
      let oneOne =
         if newFret = 1 && guitarString = 1 then 1
         else 0

      Some(" " + string x + " " + string y + " " + string (fret - baseFret + 1) + " " + string guitarString + " drawSpot ", oneOne)




(* Evaluate and create text to print for the spots of a chart
1) spots are the spots to be evaluated
2) x is the x coord
3) y is the y coord
4) text is the text that is built up and returned
5) baseFret is the value of the smallest fret
6) oneOne is 1 if there's a spot on string 1 fret 1
RETURNS a string with the text code, and 1 if string 1 fret 1 has a spot
*)
let rec evalSpots (spots: spot List) (x: float) (y: float) (text: String) (baseFret: int) (oneOne: int) : (String * int) option =

   match spots with
   | [] -> Some(text, oneOne)
   | head::tail ->

      match evalSpot head x y baseFret with
      | Some(spotText, one1) ->
         let newOneOne =
            if oneOne = 1 || one1 = 1 then 1
            else 0

         let fullText = text + spotText
         evalSpots tail x y fullText baseFret newOneOne
      | None -> None




(* Evaluate and create text to print for barre section of a chart
1) b is the barre itself
2) x is the x coord
3) y is the y coord
4) baseFret is the value of the smallest fret
RETURNS a string with the text code, and an int to say whether or not the barre started on string 1 for formatting purposes (1 yes else no)
*)
let evalBarre (b: barre) (x: float) (y: float) (baseFret: int) : (String * int) option =

   match b with
   | Barre(fret, startString, endString) ->

      let length = endString - startString

      match (startString, endString) with
      | (s,e) when s > 6 || s < 1 || e > 6 || e < 1 || s > e ->
         printfn "Error in the barre of a chart! The start string and end string must both be between 1 and 6, and the start string cannot be greater than the end string."
         printfn "Here is the problem barre: %A" b
         None

      | (s,e) ->
         let updatedFret = fret - baseFret + 1
         Some(" " + string x + " " + string y + " " + string updatedFret + " " + string s + " " + string e + " drawBarre ", startString)


   | EmptyBarre -> Some(" ", 0)




(* Find the lowest fret and the range of the frets
1) b is the barre
2) spots is the list of spots
RETURNS the base fret and the fret range
*)
let findBaseRange (b: barre) (spots: spot List) : int * int =

   // helper method, returns a list of the frets of the spots
   let findSpotFrets ss = List.collect (fun (Spot(spotFret, _)) -> [spotFret]) spots

   match b with
   | Barre(fret, startString, endString) ->

      // For each spot, get its fret and put it in a list
      let spotFrets = findSpotFrets spots

      let allFrets = fret::spotFrets

      let baseFret = List.min allFrets
      let range = (List.max allFrets) - baseFret

      (baseFret,range)

   | EmptyBarre ->

      let spotFrets = findSpotFrets spots
      let baseFret = List.min spotFrets
      let range = (List.max spotFrets) - baseFret
      (baseFret, range)



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
   | Chart(title, barre, spots) ->

      match findBaseRange barre spots with
      // if the range is too large, raise an error
      | (baseFret, range) when range > 4 ->
         printfn "A chart can only have a maximum fret range of 5! Here is the problem chart: %A" chart
         None

      // if the range is fine
      | (baseFret, range) ->

         match evalBarre barre x y baseFret with
         | Some(barreText, barre1) ->

            match evalSpots spots x y "" baseFret 0 with
            | Some(spotText, spot1) ->

               let one1 =
                  if spot1 = 1 || barre1 = 1 then 1
                  else 0

               let chartText = " " + string x + " " + string y + " (" + string baseFret + ") (" + string title + ") " + string one1 + " chart "

               Some(chartText + barreText + spotText)


            | None -> None
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
