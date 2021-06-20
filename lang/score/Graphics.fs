module Graphics

open System
open Types
open Properties
open Beams


// ############### DRAW THE NOTES ###################


// mutable global variable which stores the tuning
let mutable tuning = []

(* Given a string number and a pitch, figure out the fret number
1) guitarString is the int of which string this note will be on
2) pitch is the Pitch of the note
3) capo is the fret number of the capo
RETURNS an int which is the fret at which the pitch will be played on the string
*)
let calculateStringAndFret (guitarString: int) (pitch: Pitch) (capo: int) : int option =
   let num =
      match pitch with
      | E | ENat | FFlat -> 0
      | F | ESharp | FNat -> 1
      | FSharp | GFlat -> 2
      | G | GNat -> 3
      | GSharp | AFlat -> 4
      | A | ANat -> 5
      | ASharp | BFlat -> 6
      | B | BNat | CFlat -> 7
      | C | CNat | BSharp -> 8
      | CSharp | DFlat -> 9
      | D | DNat -> 10
      | DSharp | EFlat -> 11
      | _ ->
         printfn "Error in calculateStringAndFret. A note with Pitch type NoPitch should never enter this function"
         -1
   let numAfterCapo = (num + (12 - capo)) % 12
   // based on string number, adjust fret
   match guitarString with
   | 1 -> Some((numAfterCapo + tuning.[0]) % 12)
   | 2 -> Some((numAfterCapo + tuning.[1]) % 12)
   | 3 -> Some((numAfterCapo + tuning.[2]) % 12)
   | 4 -> Some((numAfterCapo + tuning.[3]) % 12)
   | 5 -> Some((numAfterCapo + tuning.[4]) % 12)
   | 6 -> Some((numAfterCapo + tuning.[5]) % 12)
   | _ ->
      printfn "Invalid string number! Must be 1-6"
      None





(* Helper which returns the string to print a single NormalGuitarNote
1) e is the eitherproperties to see if it wants to go up fret
2) x is the xcoord
3) y is the ycoord
4) guitarString is the string on the guitar, used for calculateStringAndFret
5) pitch is the Pitch of the note
6) capo is the capo for the note
RETURNS the string, or None
*)
let showNormalGuitarNote (e: EitherProperty List) (x: float) (y: float) (guitarString: int) (pitch: Pitch) (capo: int) : (string * int) option =
   match (calculateStringAndFret guitarString pitch capo) with
   | Some(fret) ->

      // See if this note wants to go up
      let fretUp =
         match (List.exists (fun e -> e = Upf) e) with
         | true ->
            match fret with
            | num when num > 8 -> fret
            | _ -> fret + 12
         | false -> fret

      // sub 2.5 and add 6 times the number of strings above 1. For placement
      let yCoord = (y - 2.3) + (6.0 * ((float guitarString) - 1.0))
      let newText = string x + " " + string yCoord + " " + string fretUp + " guitarfretnumber "
      Some(newText,fretUp)
   | None -> None





(* Helper which returns string for an X. Doesn't return option type like showNormalGuitarNote since ideally nothing should go wrong here....
1) x is the xcoord
2) y is the ycoord
3) guitarString is which string on the guitar for this note
RETURNS the string to be printed
*)
let showX (x: float) (y: float) (guitarString:int) : string =
   let yCoord = (y - 2.5) + (6.0 * ((float guitarString) - 1.0))
   string x + " " + string yCoord + " (x) guitarfretnumber "






(* Show the numbers or x for grace notes
1) x is the xcoord
2) y is the ycoord
3) els is the list of grace notes
4) updatedElements is the elements but with location updated to x,y
5) text is the list of strings to print
6) insideScale is how much to scale the widths
7) capo is the capo
RETURNS the list of strings, the list of new elements, and the new x y coords for the real note that this list of grace notes is attached to
*)
let rec showGraceNotes (x: float) (y: float) (els: Element List) (updatedElements: Element List) (text: string List) (insideScale: float) (capo: int) : (string List * Element List * float * float) option =


   (* Helper to show a grace note which is a normal guitar note
   *)
   let showNormalGraceNote (e: EitherProperty List) (guitarString: int) (pitch: Pitch) (capo: int) : (string * int) option =
      match (calculateStringAndFret guitarString pitch capo) with
      | Some(fret) ->

         // see if it wants to go up
         let fretUp =
            match (List.exists (fun e -> e = Upf) e) with
            | true ->
               match fret with
               | num when num > 8 -> fret
               | _ -> fret + 12
            | false -> fret

         let yCoord = (y - 1.5) + (6.0 * ((float guitarString) - 1.0))
         Some(string x + " " + string yCoord + " " + string fretUp + " guitarfretnumbergrace ",fretUp)
      | None -> None


   (* Helper to show a grace note which is an X
   *)
   let showXGraceNote (guitarString: int) : string =
      let yCoord = (y - 1.5) + (6.0 * ((float guitarString) - 1.0))
      string x + " " + string yCoord + " (x) guitarfretnumbergrace "


   match els with
   | [] ->
      Some(text,updatedElements,x,y)
   | head::tail ->

      // figure out the x coord for the next note
      let newX = x + (head.Width * insideScale)
      // update the element with it's proper location
      let newEl = { head with Location = (x,y) }

      match head.NoteInfo with
      // show a grace note which is a normal guitar note
      | SingleNote(NormalGuitarNote(guitarString,pitch,f,eProperties),mProperties) ->
         match (showNormalGraceNote eProperties guitarString pitch capo) with
         | Some(newText,fret) ->

            // update the element again for the fret
            let newNoteHead = SingleNote((NormalGuitarNote(guitarString,pitch,fret,eProperties)),mProperties)
            let newNewEl = { newEl with NoteInfo = newNoteHead }

            showGraceNotes newX y tail (updatedElements @ [newNewEl]) (text @ [newText]) insideScale capo
         | None -> None

      // show a grace note that's an x
      | SingleNote(X(guitarString,eProperties),mProperties) ->
         let newText = showXGraceNote guitarString

         showGraceNotes newX y tail (updatedElements @ [newEl]) (text @ [newText]) insideScale capo


      // show a grace note that's a group
      | GroupNote(sList,mProperties) ->

         (* Helper to show each note in the group
         *)
         let rec showGroupGraceHelper (sList: singleNote List) (fullText: string List) (newNotes: singleNote List) : (string List * singleNote List) option =
            match sList with
            | [] -> Some(fullText,newNotes)
            | head'::tail' ->
               match head' with

               | NormalGuitarNote(guitarString,pitch,f,eProperties) ->
                  match (showNormalGraceNote eProperties guitarString pitch capo) with
                  | Some(newText,fret) ->

                     // create the new note with updated fret
                     // update the element again for the fret
                     let newSingleNote = NormalGuitarNote(guitarString,pitch,fret,eProperties)

                     showGroupGraceHelper tail' (fullText @ [newText]) (newNotes @ [newSingleNote])
                  | None -> None

               | X(guitarString,eProperties) ->
                  let newText = showXGraceNote guitarString
                  showGroupGraceHelper tail' (fullText @ [newText]) (newNotes @ [head'])

         match (showGroupGraceHelper sList [] []) with
         | Some(groupText,newSingleNotes) ->

            // update the element with the new singlenotes
            let newNoteHead = GroupNote(newSingleNotes,mProperties)
            let newNewEl = { newEl with NoteInfo = newNoteHead }
            showGraceNotes newX y tail (updatedElements @ [newNewEl]) (text @ groupText) insideScale capo
         | None -> None

      | _ -> showGraceNotes x y tail (updatedElements @ [newEl]) text insideScale capo






(* Return a list of strings which are the postscript code to write each element
1) els is the list of Elements in the measure to be displayed
2) updatedElements is the list of elements but where the location has been updated, to be used later to draw the beams
3) measureWidth is the total width of this measure. it is used to place a whole rest in the middle of the measure
4) x is the x-coord for this note
5) y is the y-coord for this note
6) l is the list of strings that represents the elements to be displayed
   note: raster images in general PREPENDED to the list so they are displayed FIRST
7) insideScale is the scale used to change the widths
8) priorityText is text that should be written first
9) capo is the capo
RETURNS: updated list of strings to be displayed, and updated list of elements
*)
let rec showElements (els: Element List) (updatedElements: Element List) (measureWidth: float) (x: float) (y: float) (l: string List) (insideScale: float) (priorityText: string List) (capo: int) : (string List * string List * Element List) option =
   match els with
   | [] -> Some(l,priorityText,updatedElements)
   | head::tail ->

      // First, write out the comment
      let comment = string (x) + " " + string (y) + " (" + head.Comments + ") comment "

      // Depending on what type of element is to be written
      match head.NoteInfo with

      // Write the new time signature
      | TimeChange(top,bottom) ->

         let timeSigString = string (x + 5.0) + " " + string (y + 15.0) + " " + string top + " timesignature " + string (x + 5.0) + " " + string (y + 6.0) + " " + string bottom + " timesignature "

         let newElement = { head with Location = (x,y) }
         let newUpdatedElements = updatedElements @ [newElement]

         // x coord of next element
         let newX = x + (head.Width * insideScale)

         showElements tail newUpdatedElements measureWidth newX y l insideScale (priorityText @ [timeSigString]) capo

      // Do nothing
      | Buffer ->
         let newText = " "
         // update element with location
         let newElement = { head with Location = (x,y) }
         let newUpdatedElements = updatedElements @ [newElement]

         showElements tail newUpdatedElements measureWidth x y l insideScale priorityText capo

      // Do nothing, just move forward 5 units
      | Empty ->
         // update element with location
         let newElement = { head with Location = (x,y) }
         let newUpdatedElements = updatedElements @ [newElement]
         // just add 5, since it shouldn't be scaled
         showElements tail newUpdatedElements measureWidth (x + 5.0) y l insideScale priorityText capo
      // Guitar note: although raster image, still put at the end of the list because i want the white border

      | SingleNote(n,mProperties) ->
         match head.GraceNotes with
         // if the element has no grace notes, just display it normally
         | [] ->
            match n with

            // if it's a guitar note
            | NormalGuitarNote(guitarString,pitch,fret,eProperties) ->
               // call the helper, which calculates the fret and returns the string to print the note
               match (showNormalGuitarNote eProperties x y guitarString pitch capo) with
               | Some(newText,newFret) ->
                  // x coord of next element
                  let newX = x + (head.Width * insideScale)
                  // add string to the list
                  let newList = l @ [newText] @ [comment]
                  // update the NoteInfo with the new fret and Location
                  let newNoteHead = SingleNote((NormalGuitarNote(guitarString,pitch,newFret,eProperties)),mProperties)
                  let newElement = { head with Location = (x,y) ; NoteInfo = newNoteHead }
                  // add new element into list
                  let newUpdatedElements = updatedElements @ [newElement]
                  // recurse
                  showElements tail newUpdatedElements measureWidth newX y newList insideScale priorityText capo
               | None -> None

            | X(guitarString,eProperties) ->
               // call helper function which returns the string
               let newText = showX x y guitarString
               // x coord of the next element
               let newX = x + (head.Width * insideScale)
               let newList = l @ [newText] @ [comment]
               // updated element with location
               let newElement = { head with Location = (x,y) }
               let newUpdatedElements = updatedElements @ [newElement]
               showElements tail newUpdatedElements measureWidth newX y newList insideScale priorityText capo

         // if it does have grace notes, show those first
         | grace ->
            match n with
            // normal guitar note grace note
            | NormalGuitarNote(guitarString,pitch,fret,eProperties) ->
               // call the grace note helper
               match (showGraceNotes x y grace [] [] insideScale capo) with
               // returns the new text, the updated grace notes, and the new x y coords
               | Some(newText,newGraceNotes,newX,newY) ->

                  match (showNormalGuitarNote eProperties newX newY guitarString pitch capo) with
                  | Some(newerText,newFret) ->
                     // NOTE: adding from the original x to make the math easier and safer
                     let newerX = x + (head.Width * insideScale)
                     // add string to the list
                     let newList = l @ newText @ [newerText] @ [comment]
                     // update the NoteInfo with the new fret and Location
                     let newNoteHead = SingleNote((NormalGuitarNote(guitarString,pitch,newFret,eProperties)),mProperties)
                     let newElement = { head with Location = (newX,newY) ; NoteInfo = newNoteHead ; GraceNotes = newGraceNotes }
                     // add new element into list
                     let newUpdatedElements = updatedElements @ [newElement]
                     // recurse
                     showElements tail newUpdatedElements measureWidth newerX y newList insideScale priorityText capo
                  | None -> None
               | None -> None

            // x note
            | X(guitarString,eProperties) ->
               // still call the grace note helper in the same way
               match (showGraceNotes x y grace [] [] insideScale capo) with
               | Some(newText,newGraceNotes,newX,newY) ->
                  // should always work so not option type
                  let newerText = showX newX newY guitarString
                  // x coord of the next element
                  let newerX = x + (head.Width * insideScale)
                  let newList = l @ newText @ [newerText] @ [comment]
                  // updated element with location and grace notes
                  let newElement = { head with Location = (newX,newY); GraceNotes = newGraceNotes }
                  let newUpdatedElements = updatedElements @ [newElement]
                  showElements tail newUpdatedElements measureWidth newerX y newList insideScale priorityText capo
               | None -> None

      // show a group
      | GroupNote(nList,mProperties) ->

         // helper method to show all the notes within a group
         let rec groupHelper (nList: singleNote List) (stringList: string List) (capo: int) (x:float) (y:float) (newNotes: singleNote List) : (string List * singleNote List) option =
            match nList with
            | [] -> Some(stringList,newNotes)
            | head::tail ->
               match head with
               // if it's a note
               | NormalGuitarNote(guitarString,pitch,fret,eProperties) ->
                  match (showNormalGuitarNote eProperties x y guitarString pitch capo) with
                  | Some(newText,newFret) ->
                     let newNote = NormalGuitarNote(guitarString,pitch,newFret,eProperties)
                     groupHelper tail (stringList @ [newText]) capo x y (newNotes @ [newNote])
                  | None -> None
               // if it's an X
               | X(guitarString,eProperties) ->
                  let newText = showX x y guitarString
                  groupHelper tail (stringList @ [newText]) capo x y (newNotes @ [head])

         // check to see if there are grace notes
         match head.GraceNotes with
         // no grace notes
         | [] ->
            match (groupHelper nList [] capo x y []) with
            | Some(newText,newSingleNotes) ->
               let newX = x + (head.Width * insideScale)
               let newList = l @ newText @ [comment]
               let newNoteHead = GroupNote(newSingleNotes,mProperties)
               let newElement = { head with Location = (x,y) ; NoteInfo = newNoteHead }
               let newUpdatedElements = updatedElements @ [newElement]
               showElements tail newUpdatedElements measureWidth newX y newList insideScale priorityText capo
            | None -> None
         // grace notes
         | grace ->
            match (showGraceNotes x y grace [] [] insideScale capo) with
            | Some(newText,newGraceNotes,newX,newY) ->
               // call the helper to get the strings for each note, using the new x and y
               match (groupHelper nList [] capo newX newY []) with

               | Some(newerText,newSingleNotes) ->
                  let newerX = x + (head.Width * insideScale)
                  let newList = l @ newText @ newerText @ [comment]
                  let newNoteHead = GroupNote(newSingleNotes,mProperties)
                  let newElement = { head with Location = (newX,newY); GraceNotes = newGraceNotes; NoteInfo = newNoteHead }
                  let newUpdatedElements = updatedElements @ [newElement]
                  showElements tail newUpdatedElements measureWidth newerX y newList insideScale priorityText capo
               | None -> None
            | None -> None

      | TupletNote(tupletNotes) ->
         // recurse over each note within the tuplet
         match (showElements tupletNotes [] measureWidth x y l insideScale priorityText capo) with
         | Some(newStringList, newPriorityText, newTupletNotes) ->

            // Create the new tupletNote, and update everything like for other notes
            let newerX = x + (head.Width * insideScale)
            let newList = l @ newStringList @ [comment]
            let newNoteHead = TupletNote(newTupletNotes)
            let newElement = { head with NoteInfo = newNoteHead }
            let newUpdatedElements = updatedElements @ [newElement]
            showElements tail newUpdatedElements measureWidth newerX y newList insideScale newPriorityText capo

         | None -> None


      // Barline : print the vertical line
      | Barline ->
         // subtract 5 because the last note of the measure looks 5 into the future for placement
         let barline = string (x - 5.0) + " " + string y + " 30.4 0.7 barline "
         let newList = l @ [barline]
         // thus, take 5 away for its location
         let newElement = { head with Location = (x-5.0,y) }
         let newUpdatedElements = updatedElements @ [newElement]
         // same x and y since it has no width
         showElements tail newUpdatedElements measureWidth x y newList insideScale priorityText capo
      // Rest : depending on rhythm, use the right rest and right number of dots
      | Rest ->
         let newText =
            match head.Duration with
            | R(X0,n) ->
               // place the whole rest in the middle of the measure
               let xCoord = x + (measureWidth / 2.0) - 7.0
               let yCoord = y + 15.8
               // just the rest, no dots allowed
               string xCoord + " " + string yCoord + " halfWholeRest "
            | R(X1,n) ->
               let yCoord = y + 15.8
               let rest = string x + " " + string yCoord + " halfWholeRest "
               match n with
               | 0 -> rest
               | 1 -> dotTemplate (x + 6.3) (y + 20.0) false + rest
               | 2 -> dotTemplate (x + 6.3) (y + 20.0) false + dotTemplate (x + 8.0) (y + 20.0) false + rest
               | _ -> dotTemplate (x + 6.3) (y + 20.0) false + dotTemplate (x + 8.0) (y + 20.0) false + dotTemplate (x + 9.7) (y + 20.0) false + rest
            | R(X2,n) ->
               let yCoord = y + 11.9
               let rest = string x + " " + string yCoord + " halfWholeRest "
               match n with
               | 0 -> rest
               | 1 -> dotTemplate (x + 6.3) (y + 14.5) false + rest
               | 2 -> dotTemplate (x + 6.3) (y + 14.5) false + dotTemplate (x + 8.0) (y + 14.5) false + rest
               | _ -> dotTemplate (x + 6.3) (y + 14.5) false + dotTemplate (x + 8.0) (y + 14.5) false + dotTemplate (x + 9.7) (y + 14.5) false + rest
            | R(X4,n) ->
               let yCoord = y + 9.0
               let rest = string x + " " + string yCoord + " quarterRest "
               match n with
               | 0 -> rest
               | 1 -> dotTemplate (x + 4.7) (y + 15.7) false + rest
               | 2 -> dotTemplate (x + 4.7) (y + 15.7) false + dotTemplate (x + 6.4) (y + 15.7) false + rest
               | _ -> dotTemplate (x + 4.7) (y + 15.7) false + dotTemplate (x + 6.4) (y + 15.7) false + dotTemplate (x + 8.1) (y + 15.7) false + rest
            | R(X8,n) ->
               let yCoord = y + 11.0
               let rest = string x + " " + string yCoord + " 8thRest "
               match n with
               | 0 -> rest
               | 1 -> dotTemplate (x + 5.3) (y + 16.0) false + rest
               | 2 -> dotTemplate (x + 5.3) (y + 16.0) false + dotTemplate (x + 7.0) (y + 16.0) false + rest
               | _ -> dotTemplate (x + 5.3) (y + 16.0) false + dotTemplate (x + 7.0) (y + 16.0) false + dotTemplate (x + 8.7) (y + 16.0) false + rest
            | R(X16,n) ->
               let yCoord = y + 7.0
               let rest = string x + " " + string yCoord + " 16thRest "
               match n with
               | 0 -> rest
               | 1 -> dotTemplate (x + 5.6) (y + 16.0) false + rest
               | _ -> dotTemplate (x + 5.6) (y + 16.0) false + dotTemplate (x + 7.3) (y + 16.0) false + rest
            | R(X32,n) ->
               let yCoord = y + 7.0
               let rest = string x + " " + string yCoord + " 32ndRest "
               match n with
               | 0 -> rest
               | _ -> dotTemplate (x + 6.2) (y + 20.0) false + rest
            | R(X64,n) ->
               let yCoord = y + 3.0
               string x + " " + string yCoord + " 64thRest "
            | _ -> ""
         let newX = x + (head.Width * insideScale)
         let newList = l @ [newText] @ [comment]
         // update the element with its x y coords
         let newElement = { head with Location = (x,y) }
         let newUpdatedElements = updatedElements @ [newElement]
         showElements tail newUpdatedElements measureWidth newX y newList insideScale priorityText capo






(* Show all measures of a line
1) measures is the list of SingleMeasures to be evaluated and printed
2) updatedMeasures are the measures with the new elements that have new Locations, to be used for beaming
2) x is the x-coord of the next note
3) y is the y-coord of the next note
4) l is the list of strings to be printed
5) scale is the scale of the measures - width of line / width of measures in the line
6) priorityText is text that should be written first
RETURNS: list of strings to be printed, and list of updated measures that have th new elements
*)
let rec showMeasures (measures: SingleMeasure List) (updatedMeasures: SingleMeasure List) (x: float) (y: float) (l: string List) (scale: float) (priorityText: string List) : (string List * string List * SingleMeasure List) option =
   match measures with
   | [] -> Some(l,priorityText,updatedMeasures)
   | head::tail ->
      // list of elements
      let els = head.Elements

      // new Width of the measure based on the scale
      let newWidth = head.Width * scale
      // used to scale the notes on the inside, removing the 5 units of space in the beginning
      let insideScale = newWidth / (head.Width - 5.0)

      match (showElements els [] newWidth x y l insideScale priorityText head.Capo) with
      | Some(li,priorityText',updatedElements) ->

         // x coordinate of the beginning of the next measure
         let newX = x + newWidth
         // Update the measure with the new elements
         let newMeasure = { head with Elements = updatedElements }

         // DRAW BEAMS AND PROPERTIES

         // call a helper method to traverse the elements again and beam the grace notes
         let listWithGraceBeams = beamGraceNotes newMeasure.Elements [] newMeasure.Time
         // Call the beam function, and return a new list of strings that describe how to draw the beams for that new measure
         let listWithBeams = beam newMeasure.Elements li (0.0,0.0) Other 0.0 newMeasure.Time 0 Other false

         let newUpdatedMeasures = updatedMeasures @ [newMeasure]

         // write capo change
         //todo
         // first get the location of the first element in the measure
         let changeCapoText =
            match newMeasure.Changes.Capo with

            // if the measure has a capo change
            | true ->

               let (firstX, firstY) = updatedElements.Head.Location

               [" " + string firstX + " " + string firstY + " (capo " + string newMeasure.Capo + ") changeCapo "]

            // if the measure does not have a capo change
            | false -> [""]

         showMeasures tail newUpdatedMeasures newX y (listWithBeams @ listWithGraceBeams @ changeCapoText) scale priorityText'


      | None -> None





(* Show all lines of one page
1) lines is the list of Lines to be evaluated and printed
2) updatedLines is the list of new lines with the new measures and elements
3) text is all the postscript text to be written
4) priorityText is text that should be written first
5) propertyList is the record that describes the properties to be drawn
6) tuningNumbers is the list of numbers to tweak guitar string tunings
RETURNS: new updated text, priority text, and new Line List
*)
let rec showLines (lines: Line List) (updatedLines: Line List) (text: string) (priorityText: string) (propertyList: PropertyList) (tuningNumbers: int List) : (string * string * Line List) option =

   // set global varible
   tuning <- tuningNumbers

   match lines with
   // Base case: return the text when all lines have been processed
   | [] -> Some(text,priorityText,updatedLines)
   // Recursive case
   | head::tail ->
      // Get x and y coordinates of the beginning of the line
      let (staffx,staffy) = head.Start

      // Call guitartablines to create lines, based on start position
      let staffline =
         match staffx with
         // If first line, pass "1" to the function to specify length
         | 70.0 -> string staffx + " " + string staffy + " 1 guitartablines"
         | _ -> string staffx + " " + string staffy + " 0 guitartablines"

      // Create clef and/or time signature
      let (clef, timeSig, newX) =

         match head.LineNumber with

         // If it's the first line, add a clef AND time signature
         | 1 ->
            let clefString = string (staffx + 3.0) + " " + string (staffy + 2.0) + " 7 25.2 70 252 (images/Staves/Staff.jpg) 3 printimage "
            // Time signature depends on the time sig of the first measure of that line
            let (currentTime1, currentTime2) = head.Measures.Head.Time
            let timeSigString = string (staffx + 14.0) + " " + string (staffy + 15.0) + " " + string currentTime1 + " timesignature " + string (staffx + 14.0) + " " + string (staffy + 6.0) + " " + string currentTime2 + " timesignature "
            (clefString, timeSigString, staffx + 30.0)

         // For all other lines, just add the clef
         | _ ->
            let clefString = string (staffx + 3.0) + " " + string (staffy + 2.0) + " 7 25.2 70 252 (images/Staves/Staff.jpg) 3 printimage "
            (clefString, "", staffx + 20.0)

      let newHead : Line =

         // If the line isn't very full, add some empty measures
         match head.OriginalWidth with

         // Add 3 empty measures if less than 25% full
         | num when num <= (head.FinalWidth / 4.0) ->
            let oldMeasures = head.Measures
            let newMeasures = oldMeasures @ [emptyMeasure] @ [emptyMeasure] @ [emptyMeasure] @ [emptyMeasure] @ [emptyMeasure]
            { head with Measures = newMeasures; OriginalWidth = num + 175.0 }

         // Add 2 empty measures if 25-50% full
         | num when num > (head.FinalWidth / 4.0) && num <= (head.FinalWidth / 2.0) ->
            let oldMeasures = head.Measures
            let newMeasures = oldMeasures @ [emptyMeasure] @ [emptyMeasure] @ [emptyMeasure]
            { head with Measures = newMeasures; OriginalWidth = num + 105.0 }

         // Add 1 empty measure if 50-75% full
         | num when num > (head.FinalWidth / 2.0) && num <= (head.FinalWidth * (3.0/4.0)) ->
            let oldMeasures = head.Measures
            let newMeasures = oldMeasures @ [emptyMeasure]
            { head with Measures = newMeasures; OriginalWidth = num + 35.0 }

         | _ -> head

      // Float to tell how much to scale widths of individual elements
      let scale = (newHead.FinalWidth + staffx - newX) / newHead.OriginalWidth
      // Show measures of the line

      match (showMeasures newHead.Measures [] newX staffy [] scale []) with
      | Some(li,priorityText',updatedMeasures) ->

         // Put all the strings together
         let allNewElements = staffline + (List.fold (fun acc elem -> acc + " " + elem) "" li)

         // Put all priority strings together
         let allPriorityElements = List.fold (fun acc elem -> acc + " " + elem) "" priorityText'

         // Add the measure number of the first measure of the line
         // Find the measure number
         let firstMeasureNumber = updatedMeasures.Head.MeasureNumber

         // Add the string
         let measureNumberString = string (staffx - 5.0) + " " + string (staffy + 40.0) + " (" + string firstMeasureNumber + ") measureNumber "
         let newText = text + clef + timeSig + allNewElements + measureNumberString
         let newPriorityText = priorityText + allPriorityElements

         // Update the line with the new measures
         let newLine = { head with Measures = updatedMeasures }
         let newUpdatedLines = updatedLines @ [newLine]

         // draw the properties. This is done from lines because slurs and ties can extend across lines
         match (drawProperties newLine.Measures [] propertyList) with
         | Some(propertyText,newPropertyList) ->

            // CHECK ENDINGS
            // check for ending slurs
            match (checkEndSlur tail newPropertyList) with
            | Some(slurText,newPropertyList') ->

               // check for ending ties
               match (checkEndTie tail newPropertyList') with
               | Some(tieText,newPropertyList'') ->

                  match (checkEndSlide tail newPropertyList'') with
                  | Some(newPropertyList''') ->

                     match (checkEndHammer tail newPropertyList''') with
                     | Some(newPropertyList'''', hammerText) ->

                        match (checkEndMute tail newPropertyList'''') with
                        Some(muteText, newPropertyList''''') ->

                           showLines tail newUpdatedLines (newText + propertyText + slurText + tieText + hammerText + muteText) newPriorityText newPropertyList''''' tuningNumbers
                        | None -> None
                     | None -> None
                  | None -> None
               | None -> None
            | None -> None
         | None -> None
      | None -> None
