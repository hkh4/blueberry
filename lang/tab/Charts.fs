module Charts

open System
open TabTypes



(* Evaluate a single spot
1) s is the spot
2) x is the x coord
3) y is the y coord
4) baseFret is the value of the smallest fret
RETURNS the string with the text code, 1 if string 1 fret 1 has a spot, and the string number
*)
let evalSpot (s: spot) (x: float) (y: float) (baseFret: int) : (String * int * int) option =

   match s with
   | Spot(guitarString, fret) ->

      let newFret = fret - baseFret + 1

      // if there's a spot on string 1 fret one, need to move the fret number for formatting
      let oneOne =
         if newFret = 1 && guitarString = 1 then 1
         else 0

      Some(" " + string x + " " + string y + " " + string (fret - baseFret + 1) + " " + string guitarString + " drawSpot ", oneOne, guitarString)

   | XSpot(guitarString) ->

      Some(" " + string x + " " + string y + " " + string guitarString + " (X) drawOX ", guitarString, guitarString)



(* Draw an O for an open string
1) s is the int of the string number
2) x is the x coord
3) y is the y coord
RETURNS the string text code
*)
let drawO (s: int) (x: float) (y: float) : String =

   " " + string x + " " + string y + " " + string s + " (O) drawOX "




(* Evaluate and create text to print for the spots of a chart
1) spots are the spots to be evaluated
2) x is the x coord
3) y is the y coord
4) text is the text that is built up and returned
5) baseFret is the value of the smallest fret
6) oneOne is 1 if there's a spot on string 1 fret 1
7) stringUncovered keeps track of which string on the guitar don't have a spot, barre or x
RETURNS a string with the text code, and 1 if string 1 fret 1 has a spot
*)
let rec evalSpots (spots: spot List) (x: float) (y: float) (text: String) (baseFret: int) (oneOne: int) (stringsUncovered: int List): (String * int) option =

   match spots with
   | [] ->

      // Draw O's for all the remaining uncovered strings
      // Recursive function that calls a helper for each uncovered string and draws an O
      let rec drawOs x y stringsLeft text =

         match stringsLeft with
         | [] -> text
         | head::tail ->
            let newText = drawO head x y

            drawOs x y tail (text + newText)

      let OText = drawOs x y stringsUncovered ""

      let fullText = OText + text

      Some(fullText, oneOne)
   | head::tail ->

      match evalSpot head x y baseFret with
      | Some(spotText, one1, guitarString) ->

         let newStringsUncovered = List.filter (fun c -> not (c = guitarString)) stringsUncovered

         let newOneOne =
            if oneOne = 1 || one1 = 1 then 1
            else 0

         let fullText = text + spotText
         evalSpots tail x y fullText baseFret newOneOne newStringsUncovered
      | None -> None




(* Evaluate a single barre
1) b is the barre itself
2) x is the x coord
3) y is the y coord
4) baseFret is the value of the smallest fret
5) uncoveredStrings are all the guitar strings not yet touched by a barre
RETURNS a string with the text code, an int to say whether or not the barre started on string 1 fret 1 for formatting purposes (1 yes else no), and a list of all the guitar strings that have been covered by a barre
*)
let evalBarre (b: barre) (x: float) (y: float) (baseFret: int) (uncoveredStrings: int List) : (String * int * int List) option =

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

         let oneOne =
            if updatedFret = 1 && startString = 1 then 1
            else 0

         let allStrings = [startString .. endString]

         let newUncoveredStrings = List.filter (fun c -> not (List.exists (fun d -> d = c) allStrings)) uncoveredStrings

         Some(" " + string x + " " + string y + " " + string updatedFret + " " + string s + " " + string e + " drawBarre ", oneOne, newUncoveredStrings)


   | EmptyBarre -> Some(" ", 0, uncoveredStrings)




(* Evaluate and create text to print for barres section of a chart
1) b is the barre itself
2) x is the x coord
3) y is the y coord
4) baseFret is the value of the smallest fret
5) text is all the code text
6) oneOne is 1 if some barre touches fret 1 string 1
7) uncoveredStrings are all the strings that have not yet been touched by a barre
RETURNS a string with the text code, an int to say whether or not a barre started on string 1 fret 1 for formatting purposes (1 yes else no), and a list of all the guitar strings that have not been covered by a barre
*)
let rec evalBarres (barres: barre List) (x: float) (y: float) (baseFret: int) (text: String) (oneOne: int) (uncoveredStrings: int List) : (String * int * int List) option =

   match barres with
   | [] -> Some(text, oneOne, uncoveredStrings)

   | head::tail ->

      match evalBarre head x y baseFret uncoveredStrings with
      | Some(newText, newOneOne, newUncoveredStrings) ->

         let updatedOneOne =
            if newOneOne = 1 || oneOne = 1 then 1
            else 0

         evalBarres tail x y baseFret (text + newText) updatedOneOne newUncoveredStrings

      | None -> None




(* Find the lowest fret and the range of the frets
1) barres is the list of barres
2) spots is the list of spots
RETURNS the base fret and the fret range
*)
let findBaseRange (barres: barre List) (spots: spot List) : int * int =

   // helper method, returns a list of the frets of the spots
   let findSpotFrets ss =
      List.choose (fun s ->
         match s with
         | Spot(_,f) -> Some(f)
         | XSpot(_) -> None) ss

   // helper method, returns a list of the frets of the barres
   let findBarreFrets bb =
      List.choose (fun b ->
         match b with
         | Barre(f,_,_) -> Some(f)
         | EmptyBarre -> None) bb


   let spotFrets = findSpotFrets spots
   let barreFrets = findBarreFrets barres

   let allFrets = spotFrets @ barreFrets
   let baseFret = List.min allFrets
   let range = (List.max allFrets) - baseFret
   (baseFret, range)





(* Parse a single chart
1) charts is the list of Chords
2) text is the string to return
3) x is the xCoord
4) y is the yCoord
RETURNS a string to be printed to postscript, or none
*)
let evalChart (chart: TabExpr) (text: String) (x: float) (y: float) : String option =

   match chart with
   | Chart(title, barres, spots) ->

      match findBaseRange barres spots with
      // if the range is too large, raise an error
      | (baseFret, range) when range > 4 ->
         printfn "A chart can only have a maximum fret range of 5! Here is the problem chart: %A" chart
         None

      // if the range is fine
      | (baseFret, range) ->

         match evalBarres barres x y baseFret "" 0 [1..6] with
         | Some(barreText, barre1, stringsUncovered) ->

            match evalSpots spots x y "" baseFret 0 stringsUncovered with
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
3) x is the xCoord
4) y is the yCoord
5) count is the number of charts that have already been printed
6) page is what page it is currently printing on
RETURNS a string to be printed to postscript, plus the final x y coords
*)
let rec evalCharts (charts: TabExpr list) (text: String) (x: float) (y: float) (count: int) (page: int) : (String * float * float) option =

   match charts with
   | [] ->
      Some(text,x,y)
   | head::tail ->

      // call evalChart, then figure out variables for the next recursive call
      match evalChart head text x y with
      | Some(newText) ->

         let newCount = count + 1

         let newX =
            match (newCount % chartLineMax) with
            // if the number of charts is divisible by the max number per line, start a new line
            | 0 -> chartStartX
            | _ -> x + chartXSkip

         // if there's a new page, showPage will have the text "showpage"
         let (newY, newPage, showPage) =
            // if the number of charts is divisible by the max number per page, start a new page
            match (newCount % chartPageMax) with
            // always 2 because the first start on the first page is taken care of during the first function call
            | 0 -> (chartStart2Y, page + 1, " showpage ")
            | _ ->

               // if it's not a new page, need to see if we're going to a new line
               match (newCount % chartLineMax) with
               | 0 -> (y - chartYSkip, page, "")
               | _ -> (y, page, "")


         evalCharts tail (text + newText + showPage) newX newY newCount newPage
      | None -> None


















//
