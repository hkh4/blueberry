module ProjectInterpreter

open System
open System.IO
open ProjectParser
// ******************* EVAL *********************

// Data structures
type optionsRecord = {
   mutable Type: string
   mutable Time: int * int
   mutable Key: string
}

type Notehead =
| NormalGuitarNote of int * Pitch  // int is the string number
| Rest
| X of int
| Barline
| Empty

type Element = {
   NoteInfo : Notehead
   Duration : Rhythm
   Start: float
   Width : float
   LastNote : int
}

type SingleMeasure = {
   Key : string
   Time : int * int
   MeasureNumber : int
   Notes : Element List
   Width : float
}

type Line = {
   LineNumber: int
   Measures: SingleMeasure List
   OriginalWidth: float
   FinalWidth: float
   Type: string
   Start: float * float
}

type Page = {
   PageNumber: int
   Lines: Line List
}

///////// Useful Global Variables /////////
// Rhythms ordered in an array in order to be used to do rhythm math
let arrayOfRhythms = [|X1;X2;X4;X8;X16;X32;X64|]

// Widths of different rhythms
let widthOfRhythms =
   Map.empty.
      Add(R(X1,0),40.5).
      Add(R(X2,0),27.0).
      Add(R(X4,0),18.0).
      Add(R(X8,0),12.0).
      Add(R(X16,0),8.0).
      Add(R(X32,0),5.33333).
      Add(R(X64,0),3.555555)

let mutable defaultRhythm = R(X4,0)



// ***********************************************************
// ******************* EVALUATE OPTIONS **********************

// parse and assign the options
let parseOptions (a : Expr) (optionsR : optionsRecord) =
   match a with
   | ScoreOption(key: string,value: string) when key = "type" ->
      match value with
      | "tab" ->
         optionsR.Type <- value
         Some(true)
      | _ ->
         printfn "Valid types : tab"
         None
   | ScoreOption(key: string,value: string) when key = "time" ->
      let timeArray = value.Split('-')
      match (timeArray.Length) with
      | 2 ->
         let timeTuple = ((int timeArray.[0]),(int timeArray.[1]))
         optionsR.Time <- timeTuple
         Some(true)
      | _ ->
         printfn "Use the form (int)-(int) for the time"
         None
   | ScoreOption(key: string,value: string) when key = "key" ->
      match value with
      | "c" | "cm" | "c#" | "c#m" | "cb" | "d" | "dm" | "db" | "d#m" | "e" | "em" | "eb" | "ebm" | "f" | "fm" | "f#m" ->
         optionsR.Key <- value
         Some(true)
      | "f#m" | "g" | "gm" | "g#m" | "gb" | "a" | "am" | "a#m" | "ab" | "abm" | "b" | "bm" | "bb" | "bbm" ->
         optionsR.Key <- value
         Some(true)
      | _ ->
         printfn "Invalid key"
         None
   | _ ->
      printfn "Invalid option! Valid options are type, key, and time"
      None


// update the options record
let rec evalOption o optionsR=
   match o with
   | [] -> Some(optionsR)
   | head::tail ->
      match (parseOptions head optionsR) with
         | Some(_) -> evalOption tail optionsR
         | None -> None







// *********************************************************
// ******************** EVALUATE MEASURE *******************

// ############ Step 1: Convert the AST into a SingleMeasure List

(* Helper method to figure out width and start of a note
1) template is the Element created by evalNote which will be modified
2) r is the Duration of this Element
3) nextStart is the start spot of this element, which will be updated and returned
4) base is the RhythmNumber of the time signature, this constitutes one beat
RETURNS new Element and the new nextStart for the next element
*)
let widthStart (template: Element) (r: Rhythm) (nextStart: float) (baseBeat: RhythmNumber) (last: int) : (Element * float) option =
   let indexOfBeat = Array.findIndex (fun elem -> elem = baseBeat) arrayOfRhythms
   let rNumber =
     match r with
     | R(a,b) -> a
     | Other -> X4 // NOT YET IMPLEMENTED
   let indexOfRhythm = Array.findIndex (fun elem -> elem = rNumber) arrayOfRhythms
   // TODO: figure out dotted rhythms
   let differenceOfRhythm = indexOfBeat - indexOfRhythm
   let newWidthTemp = widthOfRhythms.[r]
   let newWidth =
      match last with
      | 0 -> newWidthTemp
      | _ ->
         match newWidthTemp with
         | num when num < 10.0 -> 10.0
         | _ -> newWidthTemp
   let newNextStart = nextStart + (2.0**(float differenceOfRhythm))
   Some({ template with Width = newWidth },newNextStart)



// Evaluate a single note
// Given a note, the options record, and a default rhythm to use for simples
let evalNote (measureNumber: int) (n: Note) (baseBeat: RhythmNumber) (numberOfBeats: int) (nextStart: float) (last: int) : (Element * float) option =
   let note =
      match n with // figure out the type of the note
      | Simple(p) ->
         match p with
         // Single Simple
         | SingleSimple(string,pitch,properties) ->
            let nInfo = NormalGuitarNote(string,pitch)
            { NoteInfo = nInfo; Start = nextStart; Duration = defaultRhythm; Width = 15.0; LastNote = 0 }
            // Rest Simple
         | RestSimple ->
            { NoteInfo = Rest; Start = nextStart; Duration = defaultRhythm; Width = 15.0; LastNote = 0 }
      | Complex(p) ->
         match p with
         // Single Complex
         | SingleComplex(string,pitch,r,properties) ->
            let nInfo = NormalGuitarNote(string,pitch)
            defaultRhythm <- r
            { NoteInfo = nInfo; Start = nextStart; Duration = r; Width = 15.0; LastNote = 0 }
         // Rest Complex
         | RestComplex(r) ->
            defaultRhythm <- r
            { NoteInfo = Rest; Start = nextStart; Duration = r; Width = 15.0; LastNote = 0 }
   // TODO: change width for certain note rhythms
   match (widthStart note (note.Duration) nextStart baseBeat last) with
   | Some(newNote,newNextStart) ->
      printfn "%A" newNote
      printfn "%A" newNextStart
      printfn "%A" last
      match last with
      | 1 ->
         match newNextStart with
         | num when num = float numberOfBeats + 1.0 -> Some({ newNote with LastNote = 1 },newNextStart)
         | num when num > float numberOfBeats + 1.0 ->
            printfn "Error! Too many beats in measure %i" measureNumber
            None
         | _ ->
            printfn "Error! Not enough beats in measure %i" measureNumber
            None
      | _ ->
         match newNextStart with
         | num when num >= float numberOfBeats + 1.0 ->
            printfn "Error! Too many beats in measure %i" measureNumber
            None
         | _ -> Some({ newNote with LastNote = 1 },newNextStart)
   | None -> None


// Recursive helper for measure evaluator, calls the note evaluator and composes the list of Elements, returns a SingleMeasure
let rec evalMeasureHelper (measureNumber: int) (m : Note List) (elementList : Element List) (baseBeat: RhythmNumber) (numberOfBeats: int) (acc : float) (nextStart: float) : (float * Element List) option =
   match m with
   | [] -> Some(acc, elementList)
   | head::tail ->
      // create a new list that contains the new note evaluated added onto all the others
      let el =
         // if tail is empty, then this note is the last one
         match tail with
         | [] -> evalNote measureNumber head baseBeat numberOfBeats nextStart 1
         | _ -> evalNote measureNumber head baseBeat numberOfBeats nextStart 0
      match el with
      | Some(n,newNextStart) ->
         // keep track of the total width of the measure
         let newAcc = n.Width + acc
         let newList = elementList @ [n]
         evalMeasureHelper measureNumber tail newList baseBeat numberOfBeats newAcc newNextStart
      | None -> None



// Evaluate a single measure
let evalMeasure (m: Expr) (optionsR: optionsRecord) : SingleMeasure option =
   match m with
   // b is measure number, c is Note List
   | Measure(b,c) ->
      let elementList : Element List = []
      let acc = 0.0
      let (numberOfBeats,baseNumber) = optionsR.Time
      let baseBeat =
         match baseNumber with
         | 4 -> X4
         | _ -> X4 //NOT YET IMPLEMENTED
      match (evalMeasureHelper b c elementList baseBeat numberOfBeats acc 1.0) with
      // tuple: first element is the total width of all the elements added together, second is the list of elements
      | Some(width,list) ->
         // Add empty space at the beginning and barline at the end
         let empty = { NoteInfo = Empty; Start = 0.0; Duration = Other; Width = 5.0; LastNote = 0 }
         let bar = { NoteInfo = Barline; Start = 0.0; Duration = Other; Width = 0.0; LastNote = 0 }
         let newList = [empty] @ list @ [bar]
         let newWidth = width + 5.0
         // create instance of SingleMeasure
         let mes = { Time = optionsR.Time; Key = optionsR.Key; MeasureNumber = b; Notes = newList; Width = newWidth }
         Some(mes)
      | None -> None
   | _ ->
      printfn "Something is very wrong. Given ScoreOption instead of Measure"
      None


// Goes through each measure one at a time, gives to evalMeasure, returns list of SingleMeasure
let rec evalAllMeasures(measuresList : Expr List) (optionsR : optionsRecord) (singleMeasureList : SingleMeasure List) : SingleMeasure List option =
   match measuresList with
   // Base case : return SingleMeasure List
   | [] -> Some(singleMeasureList)
   | head::tail ->
      // Create a single SingleMeasure
      match (evalMeasure head optionsR) with
      | Some(m) ->
         // Concatenate and recurse on tail
         let newList = singleMeasureList @ [m]
         evalAllMeasures tail optionsR newList
      | None -> None






// ############### Step 2: Divide the measures into lines

// Returns a Line
let rec divideSingleLine (measureList : SingleMeasure List) (measuresSoFar : SingleMeasure List) (widthPerLine : float) (total : float) : SingleMeasure List option =
   match measureList with
   // Base case : all measures have been consumed
   | [] -> Some(measuresSoFar)
   | head::tail ->
      // Add the total width so far with the width of the next measure
      let newTotal = total + head.Width
      match newTotal with
      // If the Line isn't full, recurse to add more
      | num when num < widthPerLine ->
         let newList = measuresSoFar @ [head]
         divideSingleLine tail newList widthPerLine newTotal
      // If full, return
      | _ -> Some(measuresSoFar)


// Divide the SingleMeasure List into Line List
let rec divideLines (measureList : SingleMeasure List) (lineList : Line List) (widthPerLine : float) : Line List option =
   match measureList with
   // Base case : all measures have been consumed
   | [] -> Some(lineList)
   // Recurse
   | head::tail ->
      let measuresSoFar : SingleMeasure List = []
      // Returns a Line
      match (divideSingleLine measureList measuresSoFar widthPerLine 0.0) with
         | Some(l) ->
            // Figure out the index of the next element after the list returned, which is the starting index for the next line
            let nextMeasure = l.Length
            // Add the line number into the Line object
            let lineNumber = lineList.Length + 1
            // Add up the widths of all the SingleMeasure within the Line
            let originalWidth = List.fold (fun acc elem -> acc + elem.Width) 0.0 l
            // Create Line. Set start to 0,0 for now as a placeholder
            let newLine = { LineNumber = lineNumber; Measures = l; OriginalWidth = originalWidth; FinalWidth = widthPerLine; Type = "tab"; Start = (0.0,0.0)}
            let newList = lineList @ [newLine]
            let measuresRemaining = measureList.[nextMeasure..]
            //515 is the width of all lines except the first, which is 495
            divideLines measuresRemaining newList 515.0
         | None -> None







// ################# Step 3: Divide lines into pages

// create one page
let rec divideOnePage (lines : Line List) (linesSoFar : Line List) (start : float * float) : Line List option =
   match lines with
   // base case: return list of lines that will make up this page
   | [] -> Some(linesSoFar)
   | head::tail ->
      // start coordinates of the next line
      let (x,y) = start
      match (head.Type) with
      | "tab" ->
         // subtract 70 to be the start of the next line
         let newY = y - 70.0
         match newY with
         // 60 is the lowest a line can start at the bottom of a page
         | num when num >= 60.0 ->
            let newLine = { head with Start = (x,newY) }
            let newList = linesSoFar @ [newLine]
            divideOnePage tail newList (50.0,newY)
         | _ -> Some(linesSoFar)
      | _ -> None


let rec dividePages (lines : Line List) (pageList : Page List) (start : float * float) : Page List option =
   match lines with
   // base case: return list of pages
   | [] -> Some(pageList)
   | head::tail ->
      let linesSoFar : Line List = []
      match (divideOnePage lines linesSoFar start) with
      // returns a Line List
      | Some(l) ->
         // figure out which is the next line for the start of the next page
         let nextLine = l.Length
         // page number
         let pageNumber = pageList.Length + 1
         let newPage = { PageNumber = pageNumber; Lines = l }
         // add this page to the list
         let newList = pageList @ [newPage]
         // split line list
         let linesRemaining = lines.[nextLine..]
         // 770, so that 770-70=700 is the start of the next line for all pages except the first, which instead starts at 720-70=650
         dividePages linesRemaining newList (50.0,770.0)
      | None -> None









// ****************************************************************
// **************************** GRAPHICS *****************************

// Given a string number and a pitch, figure out the fret number
let calculateStringAndFret (guitarString: int) (pitch: Pitch) : int option =
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
   // based on string number, adjust fret
   match guitarString with
   | 1 | 6 -> Some(num)
   | 2 -> Some((num + 7) % 12)
   | 3 -> Some((num + 2) % 12)
   | 4 -> Some((num + 9) % 12)
   | 5 -> Some((num + 5) % 12)
   | _ ->
      printfn "Invalid string number! Must be 1-6"
      None



// Return a list of strings which are the postscript code to write each element
let rec showElements (els: Element List) (x: float) (y: float) (l: string List) (insideScale: float) : string List option =
   match els with
   | [] -> Some(l)
   | head::tail ->
      // Depending on what type of element is to be written
      match head.NoteInfo with
      // Do nothing, just move forward 5 units
      | Empty ->
         showElements tail (x + 5.0) y l insideScale
      // Guitar note: RASTER IMAGE and so PREPEND to list, so it'll be printed out FIRST
      | NormalGuitarNote(guitarString,pitch) ->
         match (calculateStringAndFret guitarString pitch) with
         | Some(fret) ->
            let yCoord = (y - 2.5) + (6.0 * ((float guitarString)-1.0))
            let newText = string x + " " + string yCoord + " " + string fret + " guitarfretnumber "
            let newX = x + (head.Width * insideScale)
            let newList = l @ [newText]
            showElements tail newX y newList insideScale
         | None -> None
      | Barline ->
         let barline = string (x - 5.0) + " " + string y + " 30.4 0.7 barline "
         let newList = l @ [barline]
         showElements tail x y newList insideScale
      | Rest -> // NOT YET IMPLEMENTED
         showElements tail x y l insideScale
      | X(string) -> // NOT YET IMPLEMENTED
         showElements tail x y l insideScale
      // TODO::: FOR OTHERS : for raster image items, like clefs, they need to be PREPENDED to the list so that when evaluated, they are printed FIRST



// Show all measures of a line
let rec showMeasures (measures: SingleMeasure List) (x: float) (y: float) (l: string List) (scale: float) : string List option =
   match measures with
   | [] -> Some(l)
   | head::tail ->
      // list of elements
      let els = head.Notes
      // new Width of the measure based on the scale
      let newWidth = head.Width * scale
      // used to scale the notes on the inside, removing the 5 units of space in the beginning
      // TODO : add -5.0 after newWidth if the last element has a width of less than 15
      let insideScale = System.Math.Round((newWidth / (head.Width - 5.0)),5)
      match (showElements els x y l insideScale) with
      | Some(li) ->
         // x coordinate of the beginning of the next measure
         let newX = x + newWidth
         showMeasures tail newX y li scale
      | None -> None



// Show all lines of one page
let rec showLines (lines: Line List) (text: string) : string option =
   match lines with
   // Base case: return the text when all lines have been processed
   | [] -> Some(text)
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
      // Creat clef and/or time signature
      let (clef, timeSig, newX) =
         match head.LineNumber with
         // If it's the first line, add a clef AND time signature
         | 1 ->
            let clefString = string (staffx + 3.0) + " " + string (staffy + 2.0) + " 7 25.2 70 252 (images/Staves/staff.jpg) 3 printimage "
            // Time signature depends on the time sig of the first measure of that line
            let (currentTime1, currentTime2) = head.Measures.Head.Time
            let timeSigString = string (staffx + 14.0) + " " + string (staffy + 15.0) + " " + string currentTime1 + " timesignature " + string (staffx + 14.0) + " " + string (staffy + 6.0) + " " + string currentTime2 + " timesignature "
            (clefString, timeSigString, staffx + 30.0)
         // For all other lines, just add the clef
         | _ ->
            let clefString = string (staffx + 3.0) + " " + string (staffy + 2.0) + " 7 25.2 70 252 (images/Staves/staff.jpg) 3 printimage "
            (clefString, "", staffx + 20.0)
      // Float to tell how much to scale widths of individual elements
      let scale = System.Math.Round(((head.FinalWidth + staffx - newX) / head.OriginalWidth),5)
      let l : String List = []
      // Show measures of the line
      match (showMeasures head.Measures newX staffy l scale) with
      | Some(li) ->
         // Put all the strings together
         let allNewElements = staffline + (List.fold (fun acc elem -> acc + " " + elem) "" li)
         let newText = text + clef + timeSig + allNewElements
         showLines tail newText
      | None -> None



// Driver for creating text for postscript file. Every method from here appends onto the base text, which is all the functions and other variables needed
let rec show (pages : Page List) (text: string) : string option =
   match pages with
   // Base: no more pages, print the text to a file called score.ps
   | [] ->
      File.WriteAllText("score.ps",text)
      Some(text)
   // Recursive case
   | head::tail ->
      let lines = head.Lines
      // Show the lines of a page
      match (showLines lines text) with
      | Some(t) ->
         let newText = t + " showpage "
         show tail newText
      | None -> None






// ********************* DRIVER **************************
let eval ast =
   //decompose
   let (optionsList,measuresList) = ast
   //default options
   let optionsR = {Type = "tab"; Time = (4,4); Key = "c"}
   // First, parse the options
   match (evalOption optionsList optionsR) with
   // If the options are valid, parse the measures
   | Some(_) ->
      let emptyList : SingleMeasure List = []
      // create SingleMeasure List
      match (evalAllMeasures measuresList optionsR emptyList) with
      | Some(list) ->
         // Take SingleMeasure List and use the widths to create list of lines
         let emptyLineList : Line List = []
         //495 is the width of the first line. The rest are 515.
         match (divideLines list emptyLineList 495.0) with
         | Some(lines) ->
            // Take Line List and use heights and type to divide into pages
            let emptyPageList : Page List = []
            match (dividePages lines emptyPageList (70.0,720.0)) with
            | Some(pages) ->
               //printfn "%A" pages
               let text = "%!PS
               %%BeginProlog
               /concatenate { dup length 2 index length add 1 index type /arraytype eq {array}{string} ifelse dup 0 4 index putinterval dup 4 -1 roll length 4 -1 roll putinterval } bind def
               /printimage { 8 dict begin /color exch def /pathtofile exch def /sizey exch def /sizex exch def /scaley exch def /scalex exch def /ycoord exch def /xcoord exch def gsave xcoord ycoord translate scalex scaley scale sizex sizey 8 [sizex 0 0 -1 sizey mul 0 sizey] pathtofile (r) file /DCTDecode filter false color colorimage grestore end } bind def
               /timesignature { 5 dict begin /num exch def /ycoord exch def /xcoord exch def /str (images/Time_Signature/0.jpg) def /num2 {num 48 add} bind def str 22 num2 put xcoord ycoord 7 8.909 66 84 str 3 printimage } bind def
               /staffline { 4 dict begin /first exch def /ycoord exch def /xcoord exch def /width 515 def first 1 eq {/width 495 store} {} ifelse xcoord ycoord moveto 0.4 setlinewidth width 0 rlineto stroke end } bind def
               /barline { 4 dict begin /linewidth exch def /height exch def /ycoord exch def /ycoord ycoord 0.2 sub store /xcoord exch def gsave linewidth setlinewidth xcoord ycoord moveto 0 height rlineto stroke grestore end } bind def
               /guitartablines { 5 dict begin /flag exch def /ycoord exch def /xcoord exch def gsave 1.33 setlinewidth xcoord ycoord 30.4 1.33 barline stroke 0.4 setlinewidth 0 1 5 { /num exch def xcoord num 6 mul ycoord add flag staffline } for 1.33 setlinewidth /width 515 def flag 1 eq {/width 495 store}{} ifelse xcoord width add ycoord 30.4 1.33 barline stroke xcoord ycoord 40 fancyline end } bind def
               /fancyline { 3 dict begin /height exch def /ycoord exch def /xcoord exch   def xcoord 5 sub ycoord 5 sub moveto 2.5 setlinewidth 0 height rlineto stroke newpath 0.1 setlinewidth xcoord 4 sub ycoord 5 sub moveto xcoord 2 sub ycoord 5 sub xcoord 0.5 sub ycoord 5.5 sub xcoord 2 add ycoord 8 sub curveto xcoord ycoord 4.666 sub xcoord 6 sub ycoord 3 sub 10 arct closepath fill newpath xcoord 4 sub ycoord 5 sub height add moveto xcoord 2 sub ycoord 5 sub height add xcoord 0.5 sub ycoord 4.5 sub height add xcoord 2 add ycoord 2 sub height add curveto xcoord ycoord 5.333 sub height add xcoord 6 sub ycoord 7 sub height add 10 arct closepath fill } bind def
               /guitarfretnumber { 8 dict begin /str exch def /ycoord exch def /xcoord exch def /scalex 4 def /scaley 4.51 def /sizex 800 def /sizey 902 def /filestring (temp) def str type /stringtype eq { /xcoord xcoord 0.4 sub store /filestring (images/Tab_Numbers/) str (.jpg) concatenate concatenate store /scalex 4.6 store /scaley 4.8 store /sizex 1000 store }{ str 9 gt { /xcoord xcoord 1.7 sub store /scalex 7.3 store /sizex 1460 store }{} ifelse /filestring (images/Tab_Numbers/) str (ffff) cvs (.jpg) concatenate concatenate store } ifelse xcoord ycoord scalex scaley sizex sizey filestring 1 printimage end } bind def
               %%EndProlog
               "
               //print and show
               match (show pages text) with
               | Some(t) ->
                  //let tester = string (5.0 / 2.0)
                  //File.WriteAllText("score.ps",tester)
                  Some(t)
               | None -> None
            | None -> None
         | None -> None
      | None -> None
   | None ->None
































///
