module ProjectInterpreter

open System
open System.IO
open ProjectParser
// ******************* EVAL *********************

// Data structures
type optionsRecord = {
   Type: string
   Time: int * int
   Key: string
   Capo: int
   Title: string
   Composer: string
}

type singleNote =
| NormalGuitarNote of int * Pitch * EitherProperty List
| X of int * EitherProperty List

// Types of "notes"
type Notehead =
| SingleNote of singleNote * MultiProperty List
| GroupNote of singleNote List * MultiProperty List
| Rest
| Barline
| Empty

type Element = {
   NoteInfo: Notehead
   Duration: Rhythm
   Start: float
   Width: float
   LastNote: int
   Location: float * float
   Capo: int
   GraceNotes: Notehead List
}

type SingleMeasure = {
   Key: string
   Time: int * int
   MeasureNumber: int
   Elements: Element List
   Width: float
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

///////// Useful Global Variables And Functions /////////

// Rhythms ordered in an array in order to be used to do rhythm math
let arrayOfRhythms = [|X1;X2;X4;X8;X16;X32;X64|]

// Widths of different rhythms
let widthOfRhythms =
   Map.empty.
      Add(R(X0,0),25.0).
      Add(R(X1,3),70.0).
      Add(R(X1,2),60.0).
      Add(R(X1,1),50.0).
      Add(R(X1,0),40.5).
      Add(R(X2,3),37.0).
      Add(R(X2,2),34.0).
      Add(R(X2,1),31.0).
      Add(R(X2,0),27.0).
      Add(R(X4,3),25.0).
      Add(R(X4,2),23.0).
      Add(R(X4,1),21.0).
      Add(R(X4,0),18.0).
      Add(R(X8,3),16.5).
      Add(R(X8,2),15.0).
      Add(R(X8,1),13.5).
      Add(R(X8,0),12.0).
      Add(R(X16,2),10.6).
      Add(R(X16,1),9.6).
      Add(R(X16,0),8.5).
      Add(R(X32,2),7.5).
      Add(R(X32,1),7.1).
      Add(R(X32,0),6.7).
      Add(R(X64,2),6.3).
      Add(R(X64,1),5.9).
      Add(R(X64,0),5.5)

// Changeable default
let mutable defaultRhythm = R(X4,0)

// Number of beams by rhythm
let numberOfBeams =
   Map.empty.
      Add(X8,1).
      Add(X16,2).
      Add(X32,3).
      Add(X64,4)

// Measure with just a full rest, to be used to fill a line when it's not long enough to avoid overstretched measures
// NOTE: I made the width of the whole rest 30 instead of 20 so the existing measures wouldn't be so stretched in relation to the empty ones. Need to change numbers in showLines function if this number is changed
let emptyMeasure =
   {
      Key = "c";
      Time = (4,4);
      MeasureNumber = 0;
      Elements = [{ NoteInfo = Empty; Start = 0.0; Duration = Other; Width = 5.0; LastNote = 0; Location = (0.0,0.0); Capo = 0; GraceNotes = [] };{ NoteInfo = Rest; Duration = R(X0,0); Start = 1.0; Width = 30.0; LastNote = 1; Location = (0.0,0.0); Capo = 0; GraceNotes = [] };{ NoteInfo = Barline; Start = 0.0; Duration = Other; Width = 0.0; LastNote = 0; Location = (0.0,0.0); Capo = 0; GraceNotes = [] }];
      Width = 35.0
   }


// Template for drawing dots
let dotTemplate (x: float) (y: float) : string = " 1 setlinecap 1.4 setlinewidth " + string x + " " + string y + " moveto 0 0 rlineto stroke 0 setlinecap "


// Template for drawing flags
let drawFlags (x: float) (y: float) (r: Rhythm) : string List =
   match r with
   | R(X8,n) -> ["0.9 setlinewidth 1 1 1 setrgbcolor " + string (x + 2.0) + " " + string (y + 41.0) + " moveto 0 1 rlineto stroke 0 0 0 setrgbcolor ";string (x + 1.65) + " " + string (y + 40.0) + " drawFlag "]
   // for 16th notes and shorter, need to extend the stem as well
   | R(X16,n) -> ["0.7 setlinewidth " + string (x + 2.0) + " " + string (y + 42.0) + " moveto 0 3 rlineto stroke ";string (x + 1.65) + " " + string (y + 40.0) + " drawFlag ";string (x + 1.65) + " " + string (y + 44.0) + " drawFlag "]
   | R(X32,n) -> ["0.7 setlinewidth " + string (x + 2.0) + " " + string (y + 42.0) + " moveto 0 7 rlineto stroke ";string (x + 1.65) + " " + string (y + 40.0) + " drawFlag ";string (x + 1.65) + " " + string (y + 44.0) + " drawFlag ";string (x + 1.65) + " " + string (y + 48.0) + " drawFlag "]
   | R(X64,n) -> ["0.7 setlinewidth " + string (x + 2.0) + " " + string (y + 42.0) + " moveto 0 11 rlineto stroke ";string (x + 1.65) + " " + string (y + 40.0) + " drawFlag ";string (x + 1.65) + " " + string (y + 44.0) + " drawFlag ";string (x + 1.65) + " " + string (y + 48.0) + " drawFlag ";string (x + 1.65) + " " + string (y + 52.0) + " drawFlag "]
   | _ -> [""]

// Remove duplicates from a list
let removeDuplicates (l: 'a List) =
   let rec helper l1 l2 =
      match l1 with
      | [] -> l2
      | head::tail ->
         match (List.exists (fun e -> e = head) l2) with
         | true -> helper tail l2
         | false -> helper tail (l2 @ [head])
   helper l []






// ***********************************************************
// ******************* EVALUATE OPTIONS **********************

(* parse and assign the options
1) a is an Expr which should be a ScoreOption
2) optionR is the options record to be modified
RETURNS an option, the bool is really just a placeholder
*)
let parseOptions (a : Expr) (optionsR : optionsRecord) : optionsRecord option =
   match a with
   // If type
   | ScoreOption(key: string, value: string) when key = "type" ->
      let valueTrim = value.Trim(' ')
      match valueTrim with
      | "tab" ->
         let newOption = { optionsR with Key = valueTrim }
         Some(newOption)
      | _ ->
         printfn "Valid types : tab"
         None
   // If time
   | ScoreOption(key: string, value: string) when key = "time" ->
      let valueTrim = value.Trim(' ')
      let timeArray = valueTrim.Split('-')
      // make sure it's length 2
      match (timeArray.Length) with
      | 2 ->
         // try splitting up the array, turning to int, but catch exception
         try
            let timeTuple = ((int timeArray.[0]),(int timeArray.[1]))
            // make sure the top number is valid
            match (int timeArray.[0]) with
            | num when (num >= 1 && num <= 32) || num = 64 ->
               let newOption = { optionsR with Time = timeTuple }
               // make sure the bottom number is valid, update the defaultRhythm and optionsRecord
               match (int timeArray.[1]) with
               | 1 ->
                  defaultRhythm <- R(X1,0)
                  Some(newOption)
               | 2 ->
                  defaultRhythm <- R(X2,0)
                  Some(newOption)
               | 4 ->
                  defaultRhythm <- R(X4,0)
                  Some(newOption)
               | 8 ->
                  defaultRhythm <- R(X8,0)
                  Some(newOption)
               | 16 ->
                  defaultRhythm <- R(X16,0)
                  Some(newOption)
               | 32 ->
                  defaultRhythm <- R(X32,0)
                  Some(newOption)
               | 64 ->
                  defaultRhythm <- R(X64,0)
                  Some(newOption)
               | _ ->
                  printfn "The second number of the time signature can be 1, 2, 4, 8, 16, 32, or 64"
                  None
            | _ ->
               printfn "The first number of the time signature can be 1-32 or 64"
               None
         // catch
         with
            | _ ->
               printfn "The time option should be of the form (int)-(int)"
               None
      | _ ->
         printfn "The time option should be of the form (int)-(int)"
         None
   // If key
   | ScoreOption(key: string, value: string) when key = "key" ->
      let valueTrim = value.Trim(' ')
      match valueTrim with
      | "c" | "cm" | "c#" | "c#m" | "cb" | "d" | "dm" | "db" | "d#m" | "e" | "em" | "eb" | "ebm" | "f" | "fm" | "f#m" ->
         let newOption = { optionsR with Key = valueTrim }
         Some(newOption)
      | "f#" | "g" | "gm" | "g#m" | "gb" | "a" | "am" | "a#m" | "ab" | "abm" | "b" | "bm" | "bb" | "bbm" ->
         let newOption = { optionsR with Key = valueTrim }
         Some(newOption)
      | _ ->
         printfn "Invalid key. Valid options are: c cm c# c#m cb d dm db d#m e em eb ebm f fm f#m f# g gm g#m gb a am a#m ab abm b bm bb bbm"
         None
   // If capo
   | ScoreOption(key: string, value: string) when key = "capo" ->
      let valueTrim = value.Trim(' ')
      try
         let capo = int valueTrim
         let newOption = { optionsR with Capo = capo }
         Some(newOption)
      with
         | _ ->
            printfn "The capo option must be an integer"
            None
   // Title
   | ScoreOption(key: string, value: string) when key = "title" ->
      let valueTrim = value.Trim(' ')
      let newOption = { optionsR with Title = valueTrim }
      Some(newOption)
   | ScoreOption(key: string, value: string) when key = "composer" ->
      let valueTrim = value.Trim(' ')
      let newOption = { optionsR with Composer = valueTrim }
      Some(newOption)
   | _ ->
      printfn "Invalid option! Valid options are type, key, title, composer, capo, and time"
      None



(* driver to update the options record
1) o is the list of ScoreOption
2) optionsR is the options record to be updated
RETURNS the updated options record
*)
let rec evalOption (o: Expr List) (optionsR: optionsRecord) : optionsRecord option =
   match o with
   | [] ->
      Some(optionsR)
   | head::tail ->
      match (parseOptions head optionsR) with
         | Some(newOption) -> evalOption tail newOption
         | None -> None





// *********************************************************
// ******************** EVALUATE MEASURE *******************

// ############ Step 1: Convert the AST into a SingleMeasure List

(* Helper method to figure out width and start of a note
1) template is the Element created by evalNote which will be modified
2) r is the Duration of this Element
3) nextStart is the start spot of this element, which will be updated and returned
4) baseBeat is the RhythmNumber of the time signature, this constitutes one beat
5) numberOfBeats is the top number of the time signature
6) last is an into - 1 if last note, 0 else
RETURNS: new Element and the new nextStart for the next element
*)
let widthStart (template: Element) (r: Rhythm) (nextStart: float) (baseBeat: RhythmNumber) (numberOfBeats: int) (last: int) : (Element * float) option =
   // Find the index of the baseBeat in the list of rhythms
   let indexOfBeat = Array.findIndex (fun elem -> elem = baseBeat) arrayOfRhythms
   let (rNumber,dotNumber) =
      match r with
      | R(a,b) -> a,b
      | Other ->
         printfn "Error in widthStart. The note had a rhythm of type Other"
         X4,0 // Should never reach this
   match rNumber with
   | X0 ->
      let newWidth = widthOfRhythms.[r]
      Some({ template with Width = newWidth }, (float numberOfBeats + 1.0))
   | _ ->
      // Find the index of the given rhythm in the list of rhythms
      let indexOfRhythm = Array.findIndex (fun elem -> elem = rNumber) arrayOfRhythms
      // The difference in index is used to figure out how many beats are used
      let differenceOfRhythm = indexOfBeat - indexOfRhythm
      // Look into the Map of rhythms to widths
      let newWidthTemp = widthOfRhythms.[r]
      let newWidth =
         match last with
         // If this isn't the last note, return
         | 0 -> newWidthTemp
         | _ ->
            // If it is the last note, and the width is less than 10, set it to 10 so that there's sufficient space before the next bar line
            match newWidthTemp with
            | num when num < 10.0 -> 10.0
            | _ -> newWidthTemp
      // The start of the next note is 2 ^ the difference between beat and given rhythm. e.g. if the beat is 4 and the given is 8, the difference in index is -1, and 2 ^ -1 is half a beat. If there's a dot, multiply by 1.5. 2 dots, 1.75.
      let fullNextStart = (2.0**(float differenceOfRhythm))
      // Modify the start of the next note depending on the dots
      let startWithDotsAdded =
         match dotNumber with
         | 0 -> fullNextStart
         | 1 -> fullNextStart * 1.5
         | 2 -> fullNextStart * 1.75
         | 3 -> fullNextStart * 1.875
         | _ ->
            printfn "You can only have up to 3 dots on a note"
            -1.0
      let newNextStart = nextStart + startWithDotsAdded
      // Return the new element with updated width, and the next start
      Some({ template with Width = newWidth },newNextStart)




(* takes a Property list and divdes it into EitherProperties and MultiProperties
1) properties is all the Property
2) eProperties is the list of EitherProperty
3) mProperties is the list of MultiProperty
RETURNS eProperties * mProperties
*)
let rec divideProperties (properties: Property List) (eProperties: EitherProperty List) (mProperties: MultiProperty List) : EitherProperty List * MultiProperty List =
   match properties with
   | [] -> (eProperties,mProperties)
   | head::tail ->
      match head with
      | Either(p) -> divideProperties tail (p::eProperties) mProperties
      | Multi(p) -> divideProperties tail eProperties (p::mProperties)




(* Evaluate a single note
1) measureNumber is the number of the current measure
2) baseBeat is what rhythm counts as one beat, based on bottom number of time signature
3) numberOfBeats is top number of time signature - number of beats in a measure
4) nextStart is the starting spot of the next note
5) last is either 1, meaning it's the last note, or 0 otherwise
6) optionsR is the optionsRecord
7) graceBefore is the list of grace notes that precede this note. If this note isn't a grace note, then this list is added to this element
RETURNS: an Element, a float which is the start of the next note, and the list of grace notes
*)
let evalNote (measureNumber: int) (n: Note) (baseBeat: RhythmNumber) (numberOfBeats: int) (nextStart: float) (last: int) (optionsR: optionsRecord) (graceBefore: Notehead List) : (Element * float * Notehead List) option =
   // this is EITHER a tuple of an Element and a bool (true means grace note, else not) or None
   let noteOption =
      match n with // figure out the type of the note
      | Simple(p) ->
         match p with
         // Single Simple
         | SingleSimple(guitarString,pitch,properties) ->
            // split properties
            let (eProp, mProp) = divideProperties properties [] []
            // remove duplicates
            let eProperties = removeDuplicates eProp
            let mProperties = removeDuplicates mProp
            // Notehead
            let nInfo =
               match pitch with
               // the note is an X
               | NoPitch ->
                  SingleNote(X(guitarString,eProperties),mProperties)
               // normal note
               | _ ->
                  SingleNote(NormalGuitarNote(guitarString,pitch,eProperties),mProperties)
            // Check to see if it's a grace note
            match (List.exists (fun e -> e = Gra) mProperties) with
            // not a grace note
            | false ->
               Some(({ NoteInfo = nInfo; Start = nextStart; Duration = defaultRhythm; Width = 0.0; LastNote = 0; Location = (0.0,0.0); Capo = optionsR.Capo; GraceNotes = graceBefore }),false)
            // grace note
            | true ->
               Some(({ NoteInfo = nInfo; Start = nextStart; Duration = defaultRhythm; Width = 0.0; LastNote = 0; Location = (0.0,0.0); Capo = optionsR.Capo; GraceNotes = [] }),true)
         // Rest Simple
         | RestSimple ->
            Some({ NoteInfo = Rest; Start = nextStart; Duration = defaultRhythm; Width = 0.0; LastNote = 0; Location = (0.0,0.0); Capo = optionsR.Capo; GraceNotes = [] },false)
      | Complex(p) ->
         match p with
         // Single Complex
         | SingleComplex(guitarString,pitch,r,properties) ->
            // split properties
            let (eProp, mProp) = divideProperties properties [] []
            // remove duplicates
            let eProperties = removeDuplicates eProp
            let mProperties = removeDuplicates mProp
            // Notehead
            let nInfo =
               match pitch with
               // the note is an X
               | NoPitch ->
                  SingleNote(X(guitarString,eProperties),mProperties)
               // normal note
               | _ ->
                  SingleNote(NormalGuitarNote(guitarString,pitch,eProperties),mProperties)
            defaultRhythm <- r
            match (List.exists (fun e -> e = Gra) mProperties) with
            // not a grace note
            | false ->
               Some(({ NoteInfo = nInfo; Start = nextStart; Duration = r; Width = 0.0; LastNote = 0; Location = (0.0,0.0); Capo = optionsR.Capo; GraceNotes = graceBefore }),false)
            // grace note
            | true ->
               Some(({ NoteInfo = nInfo; Start = nextStart; Duration = r; Width = 0.0; LastNote = 0; Location = (0.0,0.0); Capo = optionsR.Capo; GraceNotes = [] }),true)
         // Rest Complex
         | RestComplex(r) ->
            // Only update default rhythm if the rhythm is NOT X0
            match r with
            | R(X0,0) ->
               Some({ NoteInfo = Rest; Start = nextStart; Duration = r; Width = 0.0; LastNote = 0; Location = (0.0,0.0); Capo = optionsR.Capo; GraceNotes = [] },false)
            | _ ->
               defaultRhythm <- r
               Some({ NoteInfo = Rest; Start = nextStart; Duration = r; Width = 0.0; LastNote = 0; Location = (0.0,0.0); Capo = optionsR.Capo; GraceNotes = [] },false)
      // Groups
      | Group(g) ->

         // recursive helper function to parse the notes within a group
         let rec groupHelper (gList: GroupSimple List) (sList: singleNote List) (usedStrings: int List) : singleNote List option =
            match gList with
            | [] -> Some(sList)
            | head::tail ->
               match head with
               | GS(guitarString,pitch,eProperties) ->
                  // check to see if this guitar string has already been used for a previous note in the group
                  match (List.exists (fun elem -> elem = guitarString) usedStrings) with
                  | true ->
                     printfn "Error! You can't specify two notes in one group that are on the same string!"
                     None
                  | false ->
                     let newSingleNote =
                        match pitch with
                        // X note
                        | NoPitch -> X(guitarString,eProperties)
                        // regular note
                        | _ -> NormalGuitarNote(guitarString,pitch,eProperties)
                     // recurse
                     groupHelper tail (sList @ [newSingleNote]) (guitarString::usedStrings)

         match g with
         // gsimple: group without the rhythm
         | GSimple(gList,mProperties) ->
            // call the helper function to parse the notes within the group
            match (groupHelper gList [] []) with
            | Some(singleNoteList) ->
               // turn it into a Group type
               let newGroup = GroupNote(singleNoteList,mProperties)
               match (List.exists (fun e -> e = Gra) mProperties) with
               // not a grace note
               | false ->
                  Some({ NoteInfo = newGroup; Start = nextStart; Duration = defaultRhythm; Width = 0.0; LastNote = 0; Location = (0.0,0.0); Capo = optionsR.Capo; GraceNotes = graceBefore },false)
               // grace note
               | true ->
                  Some({ NoteInfo = newGroup; Start = nextStart; Duration = defaultRhythm; Width = 0.0; LastNote = 0; Location = (0.0,0.0); Capo = optionsR.Capo; GraceNotes = [] },true)
            | None -> None
         // gcomplex: group with rhythm: does the same thing but uses r instead of the default rhythm
         | GComplex(gList,r,mProperties) ->
            match (groupHelper gList [] []) with
            | Some(singleNoteList) ->
               // turn it into a Group type
               let newGroup = GroupNote(singleNoteList,mProperties)
               match (List.exists (fun e -> e = Gra) mProperties) with
               // not a grace note
               | false ->
                  Some({ NoteInfo = newGroup; Start = nextStart; Duration = r; Width = 0.0; LastNote = 0; Location = (0.0,0.0); Capo = optionsR.Capo; GraceNotes = graceBefore },false)
               // grace note
               | true ->
                  Some({ NoteInfo = newGroup; Start = nextStart; Duration = r; Width = 0.0; LastNote = 0; Location = (0.0,0.0); Capo = optionsR.Capo; GraceNotes = [] },true)
            | None -> None



   match noteOption with
   | Some(note,b) when b = false ->
      // Check to see if a note has a valid number of dots. 8th notes and longer can up to 3 dots. 16th can have 2, 32nd can have 1, 64th cannot have any
      match note.Duration with
      | R(x,n) when n > 3 ->
         printfn "Notes cannot have more than 3 dots"
         None
      | R(x,n) when x = X0 && n > 0 ->
         printfn "0 rhythms cannot have dots"
         None
      | R(x,n) when x = X64 && n > 0 ->
         printfn "64th notes cannot have any dots"
         None
      | R(x,n) when x = X32 && n > 1 ->
         printfn "32nd notes can only have up to 1 dot"
         None
      | R(x,n) when x = X16 && n > 2 ->
         printfn "16th notes can only have up to 2 dots"
         None
      | _ ->
         // Call widthStart to create the note element object with updated width
         match (widthStart note (note.Duration) nextStart baseBeat numberOfBeats last) with
         | Some(newNote,newNextStart) ->
            match last with
            // If it's the last note
            | 1 ->
               match newNextStart with
               // If there are exactly the right number of beats in the measure, return
               | num when num = float numberOfBeats + 1.0 -> Some({ newNote with LastNote = 1 },newNextStart,[])
               // Too many beats
               | num when num > float numberOfBeats + 1.0 ->
                  printfn "Error! Too many beats in measure %i" measureNumber
                  None
               // Not enough beats
               | _ ->
                  printfn "Error! Not enough beats in measure %i" measureNumber
                  None
               // If it's not the last note
            | _ ->
               match newNextStart with
               // Too many beats
               | num when num >= float numberOfBeats + 1.0 ->
                  printfn "Error! Too many beats in measure %i" measureNumber
                  None
                  // Just right
               | _ -> Some({ newNote with LastNote = 1 },newNextStart,[])
         | None -> None
   | Some(note,b) ->
      Some(note,nextStart,(graceBefore @ [note.NoteInfo]))
   | None -> None




(* Recursive helper for measure evaluator, calls the note evaluator and composes the list of Elements, returns a SingleMeasure
1) measureNumber is the number of the current measure
2) m is the list of Notes remaining to be evaluated
3) elementList is the list of Elements which is built upon recursively by calling evalNote
4) baseBeat is the bottom number of time signature
5) numberOfBeats is the top number of time signature
6) acc is the accumulator to keep track of the total width of all the elements in the measure
7) nextStart is the start of the next element
8) optionsR is the optionsRecord
RETURNS: float which is the total width of the measure, and the list of elements that make up the measure
9) graceBefore is the list of notes that are grace notes for the next element
*)
let rec evalMeasureHelper (measureNumber: int) (m : Note List) (elementList : Element List) (baseBeat: RhythmNumber) (numberOfBeats: int) (acc : float) (nextStart: float)  (optionsR: optionsRecord) (graceBefore: Notehead List) : (float * Element List) option =
   match m with
   | [] -> Some(acc, elementList)
   | head::tail ->
      // create a new list that contains the new note evaluated added onto all the others
      let el =
         // if tail is empty, then this note is the last one
         match tail with
         | [] -> evalNote measureNumber head baseBeat numberOfBeats nextStart 1 optionsR graceBefore
         | _ -> evalNote measureNumber head baseBeat numberOfBeats nextStart 0 optionsR graceBefore
      match el with
      | Some(n,newNextStart,newGraceBefore) ->
         match newGraceBefore with
         // if it's empty, then the returned note was NOT a grace note
         | [] ->
            // keep track of the total width of the measure
            let newAcc = n.Width + acc
            // append new element to the end of the list
            let newList = elementList @ [n]
            evalMeasureHelper measureNumber tail newList baseBeat numberOfBeats newAcc newNextStart optionsR newGraceBefore
         | _ ->
            evalMeasureHelper measureNumber tail elementList baseBeat numberOfBeats acc newNextStart optionsR newGraceBefore
      | None -> None



(* Depending on the key, change the pitches
1) l is the list of Elements
2) key is the key of the measure
RETURNS the updated list of elements
*)
let parseKey (l: Element List) (key: string) : Element List =
   // A map which maps the pitches to be changed to what they should be changed to
   let mapOfChanges : Map<Pitch,Pitch> =
      match key with
      | "cb" | "abm" ->
         Map.empty.Add(E,EFlat).Add(A,AFlat).Add(B,BFlat)
      | "c" | "am" ->
         Map.empty
      | "c#" | "a#m" ->
         Map.empty.Add(C,CSharp).Add(D,DSharp).Add(E,ESharp).Add(F,FSharp).Add(G,GSharp).Add(A,ASharp).Add(B,BSharp)
      | "db" | "bbm" ->
         Map.empty.Add(D,DFlat).Add(E,EFlat).Add(G,GFlat).Add(A,AFlat).Add(B,BFlat)
      | "d" | "bm" ->
         Map.empty.Add(F,FSharp).Add(C,CSharp)
      | "eb" | "cm" ->
         Map.empty.Add(E,EFlat).Add(A,AFlat).Add(B,BFlat)
      | "e" | "c#m" ->
         Map.empty.Add(F,FSharp).Add(G,GSharp).Add(C,CSharp).Add(D,DSharp)
      | "f" | "dm" ->
         Map.empty.Add(B,BFlat)
      | "f#" | "d#m" ->
         Map.empty.Add(F,FSharp).Add(G,GSharp).Add(A,ASharp).Add(C,CSharp).Add(D,DSharp).Add(E,ESharp)
      | "gb" | "ebm" ->
         Map.empty.Add(G,GFlat).Add(A,AFlat).Add(B,BFlat).Add(C,CFlat).Add(D,DFlat).Add(E,EFlat)
      | "g" | "em" ->
         Map.empty.Add(F,FSharp)
      | "ab" | "fm" ->
         Map.empty.Add(A,AFlat).Add(B,BFlat).Add(D,DFlat).Add(E,EFlat)
      | "a" | "f#m" ->
         Map.empty.Add(C,CSharp).Add(F,FSharp).Add(G,GSharp)
      | "bb" | "gm" ->
         Map.empty.Add(B,BFlat).Add(E,EFlat)
      | "b" | "g#m" ->
         Map.empty.Add(C,CSharp).Add(D,DSharp).Add(F,FSharp).Add(G,GSharp).Add(A,ASharp)
      | _ ->
         printfn "Error in parseKey! Invalid key given"
         Map.empty

   (* recursive helper
   1) l is the element list
   2) updatedList is the updated elements
   3) mapOfChanges is the map by key which says which pitches to change and to what
   RETURNS the updated Element list
   *)
   let rec parseKeyHelper (l: Element List) (updatedList: Element List) (mapOfChanges: Map<Pitch,Pitch>) : Element List =
      match l with
      | [] -> updatedList
      | head::tail ->
         match head.NoteInfo with
         // if it's a single guitar note
         | SingleNote(NormalGuitarNote(s,pitch,eProperties),mProperties) ->
            // see if this pitch needs to be changed
            match mapOfChanges.TryFind pitch with
            // change the pitch
            | Some(newPitch) ->
               // update all the info for the new Element
               let newNInfo = SingleNote(NormalGuitarNote(s,newPitch,eProperties),mProperties)
               let newElement = { head with NoteInfo = newNInfo }
               let newList = updatedList @ [newElement]
               parseKeyHelper tail newList mapOfChanges
            // don't change this pitch, recurse
            | None ->
               let newList = updatedList @ [head]
               parseKeyHelper tail newList mapOfChanges
         // if it's a group, go through each element
         | GroupNote(nList,mProperties) ->

            //helper method to go through each element in the list
            let rec parseKeyGroup (nList: singleNote List) (newNList: singleNote List) : singleNote List =
               match nList with
               | [] -> newNList
               | head::tail ->
                  match head with
                  // if it's a guitar note, do the pitch change
                  | NormalGuitarNote(guitarString,pitch,eProperties) ->
                     match mapOfChanges.TryFind pitch with
                     | Some(newPitch) ->
                        let newNInfo = NormalGuitarNote(guitarString,newPitch,eProperties)
                        parseKeyGroup tail (newNList @ [newNInfo])
                     // doesn't need changing, just add to new list
                     | None -> parseKeyGroup tail (newNList @ [head])
                  // if it's an X, it has no pitch so just add it into the new list
                  | X(guitarString,eProperties) ->
                     parseKeyGroup tail (newNList @ [head])

            // call the group helper, returns the list of singleNote
            let newNList = parseKeyGroup nList []
            // turn that list into a NoteHead
            let newNInfo = GroupNote(newNList,mProperties)
            // create the new Element
            let newElement = { head with NoteInfo = newNInfo }
            // new List, recurse
            let newList = updatedList @ [newElement]
            parseKeyHelper tail newList mapOfChanges
         // for all other types of Notes, no pitch change needed
         | _ ->
            let newList = updatedList @ [head]
            parseKeyHelper tail newList mapOfChanges


   // call the helper, return its result
   parseKeyHelper l [] mapOfChanges






(* Evaluate a single measure
1) m is an Expression, which should be a Measure
2) optionsR is the options evaluated earlier, which will be added into the metadata of the measure
RETURNS: a SingleMeasure
*)
let evalMeasure (m: Expr) (optionsR: optionsRecord) : SingleMeasure option =
   match m with
   // b is measure number, c is Note List
   | Measure(b,c) ->
      let acc = 0.0
      let (numberOfBeats,baseNumber) = optionsR.Time
      // turn the number of the bottom of time signature into a RhythmNumber
      // (assuming that the bottom number will only ever be 1 2 4 8 etc)
      let baseBeat =
         match baseNumber with
         | 1 -> X1
         | 2 -> X2
         | 4 -> X4
         | 8 -> X8
         | 16 -> X16
         | 32 -> X32
         | 64 -> X64
         | _ ->
            printfn "Error! The bottom of the time signature can be 1 2 4 8 16 32 or 64"
            X0
      match (evalMeasureHelper b c [] baseBeat numberOfBeats acc 1.0 optionsR []) with
      // tuple: first element is the total width of all the elements added together, second is the list of elements
      | Some(width,list) ->
         // update the notes based on the key
         let listWithUpdatedKeys = parseKey list optionsR.Key
         // Add empty space at the beginning and barline at the end
         let empty = { NoteInfo = Empty; Start = 0.0; Duration = Other; Width = 5.0; LastNote = 0; Location = (0.0,0.0); Capo = 0; GraceNotes = [] }
         // Barline at the end of the measure
         let bar = { NoteInfo = Barline; Start = 0.0; Duration = Other; Width = 0.0; LastNote = 0; Location = (0.0,0.0); Capo = 0; GraceNotes = [] }
         let newList = [empty] @ listWithUpdatedKeys @ [bar]
         // Add 5 to the width because of the empty space at the beginning
         let newWidth = width + 5.0
         // create instance of SingleMeasure
         let mes = { Time = optionsR.Time; Key = optionsR.Key; MeasureNumber = b; Elements = newList; Width = newWidth }
         Some(mes)
      | None -> None
   | _ ->
      printfn "Something is very wrong. Given ScoreOption instead of Measure"
      None




(* Goes through each measure one at a time, gives to evalMeasure, returns list of SingleMeasure
1) measureList is the list of Expr's, should be a list of Measures
2) optionsR is the record of options evaluated earlier
3) singleMeasureList is a list of SingleMeasures that will be evaluated and appended
RETURNS: list of SingleMeasure evaluated by helpers
*)
let rec evalAllMeasures (measuresList : Expr List) (optionsR : optionsRecord) (singleMeasureList : SingleMeasure List) : SingleMeasure List option =
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

(* Returns a Line
1) measureList is the list of SingleMeasures that will be processed. When empty, the function is done and returns
2) measuresSoFar is the list of SingleMeasures that measures are added to if they will be in that line
3) widthPerLine is the maximum width allowed in a line. 495 for the first line and 515 for the rest
4) total is the total width of the measures added to measuresSoFar so far
RETURNS: list of SingleMeasure - these are the measures that will make up one line
*)
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



(* Divide the SingleMeasure List into Line List
1) measureList is the list of SingleMeasures that have not yet been evaluated
2) lineList is the list of Lines that is continually added to
3) widthPerLine is the maximum width allowed in a line. 495 for the first line and 515 for the rest
RETURNS: list of Lines
*)
let rec divideLines (measureList : SingleMeasure List) (lineList : Line List) (widthPerLine : float) : Line List option =
   match measureList with
   // Base case : all measures have been consumed
   | [] -> Some(lineList)
   // Recurse
   | head::tail ->
      // Returns a Line
      match (divideSingleLine measureList [] widthPerLine 0.0) with
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

(* recursively divide lines into a page
1) lines is the list of Lines to be evaluated
2) linesSoFar is the lines that will be added to this current Page
3) start is the x,y coords of the start of the current line
RETURNS: list of Lines that will make up one Page
*)
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



(* driver for page dividing
1) lines is the list of all Lines to be evaluated
2) pageList is the list of Pages that is created
3) start is the x,y coords of the start of the next line
RETURNS: list of Pages
*)
let rec dividePages (lines : Line List) (pageList : Page List) (start : float * float) : Page List option =
   match lines with
   // base case: return list of pages
   | [] -> Some(pageList)
   | head::tail ->
      match (divideOnePage lines [] start) with
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

// ################## DRAW THE BEAMS ###################

(* used to draw stubs at the end of a note
1) lastLocation is the x and y coords
2) lastRhythm is the Rhythm of the last note
note: does similar thing as initialStubs but the required info is figured out within the method, for simplicity
RETURNS list of strings to be printed
*)
let rec endingStubs (lastLocation: float * float) (lastRhythm: Rhythm) : string List =
   let (oldX,oldY) = lastLocation
   let (previousRhythmNumber,previousDots) =
      match lastRhythm with
      | R(x,n) -> x,n
      | _ ->
         printfn "Error in endingStubs: lastRhythm was of type Other, should be R(x,n)"
         X0,0 // SHOULD NEVER REACH THIS CASE
   let beamsOfPrevious = numberOfBeams.[previousRhythmNumber]

   // helper function to do the actual drawing
   let rec endingStubsHelper (x: float) (y: float) (toAdd: int List) (text: string List) : string List =
      match toAdd with
      | [] -> text
      | head::tail ->
         let stub =
            match head with
            | 1 ->
               [" 1.6 setlinewidth " + string (x - 1.5) + " " + string (y + 42.0) + " moveto " + string (x + 1.7) + " " + string (y + 42.0) + " lineto stroke "]
            | 2 ->
               [" 1.6 setlinewidth " + string (x - 1.5) + " " + string (y + 39.6) + " moveto " + string (x + 1.7) + " " + string (y + 39.6) + " lineto stroke "]
            | 3 ->
               [" 1.6 setlinewidth " + string (x - 1.5) + " " + string (y + 37.2) + " moveto " + string (x + 1.7) + " " + string (y + 37.2) + " lineto stroke "]
            | 4 ->
               [" 1.6 setlinewidth " + string (x - 1.5) + " " + string (y + 34.8) + " moveto " + string (x + 1.7) + " " + string (y + 34.8) + " lineto stroke "]
            | _ -> [""] //should never reach this
         endingStubsHelper x y tail (text @ stub)
   endingStubsHelper oldX oldY [1..beamsOfPrevious] []




(* used to draw stubs at the beginning of a note
1) x is the x coord
2) y is the y coord
3) toAdd is the list of ints which is the beams to draw
4) text is a string list that is composed as the function recurses
RETURNS the list of strings
*)
let rec initialStubs (x: float) (y: float) (toAdd: int List) (text: string List) : string List =
   match toAdd with
   | [] -> text
   | head::tail ->
      let stub =
         match head with
         | 1 ->
            [" 1.6 setlinewidth " + string (x + 1.65) + " " + string (y + 42.0) + " moveto " + string (x + 5.65) + " " + string (y + 42.0) + " lineto stroke "]
         | 2 ->
            [" 1.6 setlinewidth " + string (x + 1.65) + " " + string (y + 39.6) + " moveto " + string (x + 5.65) + " " + string (y + 39.6) + " lineto stroke "]
         | 3 ->
            [" 1.6 setlinewidth " + string (x + 1.65) + " " + string (y + 37.2) + " moveto " + string (x + 5.65) + " " + string (y + 37.2) + " lineto stroke "]
         | 4 ->
            [" 1.6 setlinewidth " + string (x + 1.65) + " " + string (y + 34.8) + " moveto " + string (x + 5.65) + " " + string (y + 34.8) + " lineto stroke "]
         | _ ->
            printfn "Error in initialStubs: more than 4 beams to be drawn"
            [""] //should never reach this
      initialStubs x y tail (text @ stub)




(* draws the full beams between notes
1) x is the x coord of the last note
2) newX is the x coord of the current note
3) y is the y coord
4) beamsToAdd is the list of ints which are the beams to be drawn
5) text is the string list
RETURNS a string list to be printed
*)
let rec fullBeams (x: float) (newX: float) (y: float) (beamsToAdd: int List) (text: string List) : string List =
   match beamsToAdd with
   | [] -> text
   | head::tail ->
      let stub =
         match head with
         | 1 ->
            [" 1.6 setlinewidth " + string (x + 1.65) + " " + string (y + 42.0) + " moveto " + string (newX + 2.35) + " " + string (y + 42.0) + " lineto stroke "]
         | 2 ->
            [" 1.6 setlinewidth " + string (x + 1.65) + " " + string (y + 39.6) + " moveto " + string (newX + 2.35) + " " + string (y + 39.6) + " lineto stroke "]
         | 3 ->
            [" 1.6 setlinewidth " + string (x + 1.65) + " " + string (y + 37.2) + " moveto " + string (newX + 2.35) + " " + string (y + 37.2) + " lineto stroke "]
         | 4 ->
            [" 1.6 setlinewidth " + string (x + 1.65) + " " + string (y + 34.8) + " moveto " + string (newX + 2.35) + " " + string (y + 34.8) + " lineto stroke "]
         | _ ->
            printfn "Error in fullBeams: a note can only have 1 2 3 or 4 beams"
            [""] //should never reach this
      fullBeams x newX y tail (text @ stub)



(* Used to determine the index of the last start and current start which is to be used to see if they should be beamed or not
1) key is the list of int lists that describe the time signature and how to beam notes
2) baseStart is the int that will be looked up in the key list
3) i is the index
RETURNS the index
*)
let rec findElementInKey (key: int list list) (baseStart: int) (i: int) : int option =
   match key with
   | [] ->
      printfn "Error in findElementInKey: the start of this note wasn't found in the key"
      None //shouldn't ever reach this
   | head::tail ->
      match List.tryFindIndex (fun el -> el = baseStart) head with
      | Some(v) -> Some(i)
      | None -> findElementInKey tail baseStart (i+1)




(* Figure out beaming for time signature depending on time signature
1) key is an int list list which depending on the time signature, tells hows the group beams
2) lastLocation is the x y coords of the last note
3) lastRhythm is the rhythm of the last note
4) lastStart is the start beat of the last note
5) head is the current element
6) lastBeamed is 1 if the two previous notes were beamed and the same number of lines, 2 if the two previous notes were beamed but the first had more beams, 3 if the two previous notes were beamed but the second had more beams, and 0 else
7) lastLastRhythm is two rhythms ago
RETURNS a list of strings and an int which is the next lastBeamed
*)
let beamByTime (key: int list list) (lastLocation: float * float) (lastRhythm: Rhythm) (lastStart: float) (head: Element) (lastBeamed: int) (lastLastRhythm: Rhythm) : string List * int =
   let (x,y) = lastLocation
   // decompose lastRhythm into its rhythmNumber and dots
   let (previousRhythmNumber,previousDots) =
      match lastRhythm with
      | R(x,n) -> x,n
      | _ ->
         printfn "Error in beam4: lastRhythm was of type Other, should be R(x,n)"
         X0,0 // SHOULD NEVER REACH THIS CASE
   // decompose current rhythm into its rhythmNumber and dots
   let (currentRhythmNumber,currentDots) =
      match head.Duration with
      | R(x,n) -> x,n
      | _ ->
         printfn "Error in beam4: head.Duration was of type Other, should be R(x,n)"
         X0,0 // SHOULD NEVER REACH THIS CASE

   (* helper to reuse code
   1) lastLocation is the x y coords of the last note
   2) lastRhythm is the rhythm of the last note
   3) lastStart is the start beat of the last note
   4) head is the current element
   5) lastBeamed is 1 if the two previous notes were beamed and the same number of lines, 2 if the two previous notes were beamed but the first had more beams, 3 if the two previous notes were beamed but the second had more beams, and 0 else
   6) lastLastRhythm is two rhythms ago
   RETURNS a list of strings and an int which is the next lastBeamed
   *)
   let beamByTimeHelper (lastLocation: float * float) (lastRhythm: Rhythm) (lastStart: float) (head: Element) (lastBeamed: int) (lastLastRhythm: Rhythm) : string List * int =
      // number of beams for the previous note
      let beamsOfPrevious = numberOfBeams.[previousRhythmNumber]
      // number of beams for the current note
      let beamsOfCurrent = numberOfBeams.[currentRhythmNumber]
      let (newX,newY) = head.Location
      match beamsOfPrevious with
      // if this note and the last are the same rhythm
      | num when num = beamsOfCurrent ->
         // just draw the full beams, lastBeamed = 1
         let equalBeams = fullBeams x newX y [1..num] []
         (equalBeams,1)
      // if the last note has more beams than the current
      | num when num > beamsOfCurrent ->
         let equalBeams = fullBeams x newX y [1..beamsOfCurrent] []
         match lastBeamed with
         | 0 ->
            // if the last and lastlast were not beamed, then the last note needs an initial stub since it needs more beams than the current
            let iStubs = initialStubs x y [1..num] []
            ((equalBeams @ iStubs),2)
         | 1 ->
            (equalBeams,2)
         | 2 ->
            (equalBeams,2)
         | 3 ->
            // if the last note had more beams than lastlast, some sort of stub is needed
            let lastLastRhythmNumber =
               match lastLastRhythm with
               | R(x,n) -> x
               | Other ->
                  printfn "Error in beam4! If the lastBeamed is 3, then lastLastRhythm cannot by of type Other"
                  X0
            // beams for last last
            let numberOfBeamsLastLast = numberOfBeams.[lastLastRhythmNumber]
            match numberOfBeamsLastLast with
            // if this note has as many or more beams than last last, then last gets an initial stub
            | num when num <= beamsOfCurrent ->
               let iStubs = initialStubs x y [1..beamsOfPrevious] []
               ((equalBeams @ iStubs),2)
            // if this note has less beams than last last, then last note gets an end stub
            | _ ->
               let endStubs = endingStubs lastLocation lastRhythm
               ((equalBeams @ endStubs),2)
         | _ ->
            printfn "Error in beam4: lastBeamed can only be 0 1 2 or 3"
            ([""],0) //should never reach this case
      // if this note has more beams than the last, draw just the beams, and a future case will take care of stubs
      | _ ->
         let equalBeams = fullBeams x newX y [1..beamsOfPrevious] []
         (equalBeams,3)

   let indexOfLast = findElementInKey key (int lastStart) 0
   let indexOfCurrent = findElementInKey key (int head.Start) 0
   match (indexOfLast) with
   // if they're in the same group, beam
   | num when num = indexOfCurrent ->
      beamByTimeHelper lastLocation lastRhythm lastStart head lastBeamed lastLastRhythm
   | _ ->
      // if they're not in the same group, beam if the previous element went PAST the exact start of the current beat. e.g. last start was 2.9, current start is 3.1, not exactly 3.0, so beam
      // float int would turn 1.xxx into 1.0
      let wholeNumber = float (int head.Start)
      let difference = head.Start - wholeNumber
      match difference with
      | num when num > 0.0 ->
         beamByTimeHelper lastLocation lastRhythm lastStart head lastBeamed lastLastRhythm
      | _ ->
         // may need to add a flag to the last note
         match lastBeamed with
         | 0 ->
            let flag = drawFlags x y lastRhythm
            (flag,0)
         | _ ->
            ([""],0)




(* Helper to avoid rewriting code
1) head is the current Element
2) s is the guitar string for the current element
3) lastLocation is the x y coords of the last element
4) lastRhythm is the Rhythm of the last element
5) lastStart is the start of the last element
6) timeSignature is the time signature
7) lastBeamed is 1 if the two previous notes were beamed and the same number of lines, 2 if the two previous notes were beamed but the first had more beams, 3 if the two previous notes were beamed but the second had more beams, and 0 else
8) lastLastRhythm is the Rhythm from two notes ago
RETURNS: a list of strings to be printed, and an int which is the new lastBeamed
*)
let beamHelper (head: Element) (s: int) (lastLocation: float * float) (lastRhythm: Rhythm) (lastStart: float) (timeSignature: int * int) (lastBeamed: int) (lastLastRhythm: Rhythm) : string List * int =
   let dotNumber =
      match head.Duration with
      | R(x,n) -> n
      | Other -> 0
   let (x,y) = head.Location
   let (oldX,oldY) = lastLocation
   // draw the stem, depending on which string it's on. If it's on string 6, the beam has to be a little shorter
   let stem =
      match s with
      | 6 -> "0.7 setlinewidth " + string (x + 2.0) + " " + string (y + 33.0) + " moveto 0 9 rlineto stroke "
      | _ -> "0.7 setlinewidth " + string (x + 2.0) + " " + string (y + 32.0) + " moveto 0 10 rlineto stroke "
   // draw the dots, depending on how many
   let dots =
      match dotNumber with
      | 0 -> ""
      | 1 -> dotTemplate (x + 4.2) (y + 33.4)
      | 2 -> dotTemplate (x + 4.2) (y + 33.4) + dotTemplate (x + 5.9) (y + 33.4)
      | 3 -> dotTemplate (x + 4.2) (y + 33.4) + dotTemplate (x + 5.9) (y + 33.4) + dotTemplate (x + 7.6) (y + 33.4)
      | _ ->
         printfn "Error in beamHelper! Note had more than 3 dots"
         ""
   match lastRhythm with
   // If the last note is a whole, half, or quarter note, just put the stem and dots on this note
   | R(X0,n) | R(X1,n) | R(X2,n) | R(X4,n) ->
      (([stem]@[dots]),0)
   // same thing if the last element wasn't a note
   | Other ->
      (([stem]@[dots]),0)
   // if the last note was an 8th or shorter note
   | R(x,n) ->
      match head.Duration with
      // If the current note is also an 8th or shorter note
      | R(X8,n) | R(X16,n) | R(X32,n) | R(X64,n) ->
         // The way notes are beamed depends on the time signature
         let key =
            match timeSignature with
            // if quarter, half, or whole gets a beat, each beat is in its own group
            | (n,m) when m = 1 || m = 2 || m = 4 ->
               let temp = [1..n]
               List.fold (fun acc elem -> acc @ [[elem]]) [] temp
            // for the rest, if the number of beats is even but not a multiple of 3, each beat is in its own group
            | (n,m) when (m = 8 || m = 16 || m = 32 || m = 64) && ((n % 2 = 0) && not (n % 3 = 0)) ->
               let temp = [1..n]
               List.fold (fun acc elem -> acc @ [[elem]]) [] temp
            // for multiples of 3, group in 3s
            | (n,m) when (m = 8 || m = 16 || m = 32 || m = 64) && (n % 3 = 0) ->
               let numberOfGroups = n / 3
               let temp = [1..numberOfGroups]
               List.map (fun x -> [x*3-2;x*3-1;x*3]) temp
            | _ ->
               printfn "Error in beamHelper: this time signature has not yet been implemented. Sorry!"
               [[0]]
         // call beamByTime, return the strings of the beams and the int for the new lastBeamed
         let (newText, newLastBeamed) = beamByTime key lastLocation lastRhythm lastStart head lastBeamed lastLastRhythm
         // return
         (([stem] @ newText @ [dots]),newLastBeamed)
      // If the current note is quarter half or whole
      | _ ->
         // check to see if there's an end stub to be drawn
         match lastBeamed with
         | 3 ->
            let endStubs = endingStubs lastLocation lastRhythm
            (([stem] @ endStubs @ [dots]),0)
         | 0 ->
            // check to see if flags are needed on the previous note
            let flag = drawFlags oldX oldY lastRhythm
            (([stem] @ [dots] @ flag),0)
         | _ ->
            (([stem]@[dots]),0)




(* Driver for drawing beams
1) els is the list of Elements in this measure
2) text is the list of strings
3) lastLocation is the x-y coords of the last element
4) lastRhythm is the Rhythm of the last note
5) lastStart is the start beat of the last note
6) timeSignature is the time signature
7) lastBeamed is 1 if the two previous notes were beamed and the same number of lines, 2 if the two previous notes were beamed but the first had more beams, 3 if the two previous notes were beamed but the second had more beams, and 0 else
8) lastLastRhythm is two rhythms ago
RETURNS a list of strings
*)
let rec beam (els: Element List) (text: string List) (lastLocation: float * float) (lastRhythm: Rhythm) (lastStart: float) (timeSignature: int * int) (lastBeamed: int) (lastLastRhythm: Rhythm) : string List =
   match els with
   // Base case: no more elements
   | [] ->
      // need to check if there any remaining end stubs to draw
      match lastBeamed with
      // If the lastBeamed was 3, draw the final end stub
      | 3 ->
         let endStubs = endingStubs lastLocation lastRhythm
         text @ endStubs
      | _ ->
         text
   | head::tail ->
      match head.NoteInfo with
      | SingleNote(n,mProperties) ->
         match n with
         // guitar note - beam
         | NormalGuitarNote(guitarString,pitch,eProperties) ->
            let (newText, newLastBeamed) = beamHelper head guitarString lastLocation lastRhythm lastStart timeSignature lastBeamed lastLastRhythm
            beam tail (text @ newText) head.Location head.Duration head.Start timeSignature newLastBeamed lastRhythm
         // also beam if an X
         | X(guitarString,eProperties) ->
            let (newText, newLastBeamed) = beamHelper head guitarString lastLocation lastRhythm lastStart timeSignature lastBeamed lastLastRhythm
            beam tail (text @ newText) head.Location head.Duration head.Start timeSignature newLastBeamed lastRhythm
      // If it isn't a note or an x, check if there's an end stub that needs to be drawn
      | GroupNote(nList,mProperties) ->

         // helper method to find out if any of the notes in the group are on string 6 for stem purposes
         let rec findString6 (nList: singleNote List) : bool =
            match nList with
            | [] -> false // random number that isn't 6
            | head::tail ->
               match head with
               | NormalGuitarNote(guitarString,pitch,eProperties) ->
                  match guitarString with
                  // if it's 6 return, if not, check the tail of the list
                  | 6 -> true
                  | _ -> findString6 tail
               | X(guitarString,eProperties) ->
                  match guitarString with
                  | 6 -> true
                  | _ -> findString6 tail

         let (newText, newLastBeamed) =
            match (findString6 nList) with
            // if there was a 6, then call beamHelper with the string set to 6
            | true -> beamHelper head 6 lastLocation lastRhythm lastStart timeSignature lastBeamed lastLastRhythm
            // otherwise, not 6. 1 chosen at random
            | _ -> beamHelper head 1 lastLocation lastRhythm lastStart timeSignature lastBeamed lastLastRhythm
         // recurse
         beam tail (text @ newText) head.Location head.Duration head.Start timeSignature newLastBeamed lastRhythm
      | _ ->
         match lastBeamed with
         | 3 ->
            // check if end stubs are needed if this note is not a note but the last note might need a stub
            let endStubs = endingStubs lastLocation lastRhythm
            beam tail (text @ endStubs) (0.0,0.0) Other 0.0 timeSignature 0 lastRhythm
         | 0 ->
            // last note might need flags
            let (oldX,oldY) = lastLocation
            let flag = drawFlags oldX oldY lastRhythm
            beam tail (text @ flag) (0.0,0.0) Other 0.0 timeSignature 0 lastRhythm
         | _ ->
            beam tail text (0.0,0.0) Other 0.0 timeSignature 0 lastRhythm




// ############### DRAW THE NOTES ###################

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
   | 1 | 6 -> Some(numAfterCapo)
   | 2 -> Some((numAfterCapo + 7) % 12)
   | 3 -> Some((numAfterCapo + 2) % 12)
   | 4 -> Some((numAfterCapo + 9) % 12)
   | 5 -> Some((numAfterCapo + 5) % 12)
   | _ ->
      printfn "Invalid string number! Must be 1-6"
      None



(* Helper which returns the string to print a single NormalGuitarNote
1) x is the xcoord
2) y is the ycoord
3) guitarString is the string on the guitar, used for calculateStringAndFret
4) pitch is the Pitch of the note
5) capo is the capo for the note
RETURNS the string, or None
*)
let showNormalGuitarNote (x: float) (y: float) (guitarString: int) (pitch: Pitch) (capo: int) : string option =
   match (calculateStringAndFret guitarString pitch capo) with
   | Some(fret) ->
      // sub 2.5 and add 6 times the number of strings above 1. For placement
      let yCoord = (y - 2.3) + (6.0 * ((float guitarString) - 1.0))
      let newText = string x + " " + string yCoord + " " + string fret + " guitarfretnumber "
      Some(newText)
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



(* Return a list of strings which are the postscript code to write each element
1) els is the list of Elements in the measure to be displayed
2) updatedElements is the list of elements but where the location has been updated, to be used later to draw the beams
3) measureWidth is the total width of this measure. it is used to place a whole rest in the middle of the measure
4) x is the x-coord for this note
5) y is the y-coord for this note
6) l is the list of strings that represents the elements to be displayed
   note: raster images in general PREPENDED to the list so they are displayed FIRST
7) insideScale is the scale used to change the widths
RETURNS: updated list of strings to be displayed, and updated list of elements
*)
let rec showElements (els: Element List) (updatedElements: Element List) (measureWidth: float) (x: float) (y: float) (l: string List) (insideScale: float) : (string List * Element List) option =
   match els with
   | [] -> Some(l,updatedElements)
   | head::tail ->
      // Depending on what type of element is to be written
      match head.NoteInfo with
      // Do nothing, just move forward 5 units
      | Empty ->
         // update element with location
         let newElement = { head with Location = (x,y) }
         let newUpdatedElements = updatedElements @ [newElement]
         showElements tail newUpdatedElements measureWidth (x + 5.0) y l insideScale
      // Guitar note: although raster image, still put at the end of the list because i want the white border
      | SingleNote(n,mProperties) ->
         match n with
         // if it's a guitar note
         | NormalGuitarNote(guitarString,pitch,eProperties) ->
            // call the helper, which calculates the fret and returns the string to print the note
            match (showNormalGuitarNote x y guitarString pitch head.Capo) with
            | Some(newText) ->
               // x coord of next element
               let newX = x + (head.Width * insideScale)
               // add string to the list
               let newList = l @ [newText]
               // updated element with location
               let newElement = { head with Location = (x,y) }
               // add new element into list
               let newUpdatedElements = updatedElements @ [newElement]
               // recurse
               showElements tail newUpdatedElements measureWidth newX y newList insideScale
            | None -> None
         | X(guitarString,eProperties) ->
            // call helper function which returns the string
            let newText = showX x y guitarString
            // x coord of the next element
            let newX = x + (head.Width * insideScale)
            let newList = l @ [newText]
            // updated element with location
            let newElement = { head with Location = (x,y) }
            let newUpdatedElements = updatedElements @ [newElement]
            showElements tail newUpdatedElements measureWidth newX y newList insideScale
      // show a group
      | GroupNote(nList,mProperties) ->

         // helper method to show all the notes within a group
         let rec groupHelper (nList: singleNote List) (stringList: string List) (capo: int) : string List option =
            match nList with
            | [] -> Some(stringList)
            | head::tail ->
               match head with
               // if it's a note
               | NormalGuitarNote(guitarString,pitch,eProperties) ->
                  match (showNormalGuitarNote x y guitarString pitch capo) with
                  | Some(newText) ->
                     groupHelper tail (stringList @ [newText]) capo
                  | None -> None
               // if it's an X
               | X(guitarString,eProperties) ->
                  let newText = showX x y guitarString
                  groupHelper tail (stringList @ [newText]) capo


         match (groupHelper nList [] head.Capo) with
         | Some(newText) ->
            let newX = x + (head.Width * insideScale)
            let newList = l @ newText
            let newElement = { head with Location = (x,y) }
            let newUpdatedElements = updatedElements @ [newElement]
            showElements tail newUpdatedElements measureWidth newX y newList insideScale
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
         showElements tail newUpdatedElements measureWidth x y newList insideScale
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
               | 1 -> dotTemplate (x + 6.3) (y + 20.0) + rest
               | 2 -> dotTemplate (x + 6.3) (y + 20.0) + dotTemplate (x + 8.0) (y + 20.0) + rest
               | _ -> dotTemplate (x + 6.3) (y + 20.0) + dotTemplate (x + 8.0) (y + 20.0) + dotTemplate (x + 9.7) (y + 20.0) + rest
            | R(X2,n) ->
               let yCoord = y + 11.9
               let rest = string x + " " + string yCoord + " halfWholeRest "
               match n with
               | 0 -> rest
               | 1 -> dotTemplate (x + 6.3) (y + 14.5) + rest
               | 2 -> dotTemplate (x + 6.3) (y + 14.5) + dotTemplate (x + 8.0) (y + 14.5) + rest
               | _ -> dotTemplate (x + 6.3) (y + 14.5) + dotTemplate (x + 8.0) (y + 14.5) + dotTemplate (x + 9.7) (y + 14.5) + rest
            | R(X4,n) ->
               let yCoord = y + 9.0
               let rest = string x + " " + string yCoord + " quarterRest "
               match n with
               | 0 -> rest
               | 1 -> dotTemplate (x + 4.7) (y + 15.7) + rest
               | 2 -> dotTemplate (x + 4.7) (y + 15.7) + dotTemplate (x + 6.4) (y + 15.7) + rest
               | _ -> dotTemplate (x + 4.7) (y + 15.7) + dotTemplate (x + 6.4) (y + 15.7) + dotTemplate (x + 8.1) (y + 15.7) + rest
            | R(X8,n) ->
               let yCoord = y + 11.0
               let rest = string x + " " + string yCoord + " 8thRest "
               match n with
               | 0 -> rest
               | 1 -> dotTemplate (x + 5.3) (y + 16.0) + rest
               | 2 -> dotTemplate (x + 5.3) (y + 16.0) + dotTemplate (x + 7.0) (y + 16.0) + rest
               | _ -> dotTemplate (x + 5.3) (y + 16.0) + dotTemplate (x + 7.0) (y + 16.0) + dotTemplate (x + 8.7) (y + 16.0) + rest
            | R(X16,n) ->
               let yCoord = y + 7.0
               let rest = string x + " " + string yCoord + " 16thRest "
               match n with
               | 0 -> rest
               | 1 -> dotTemplate (x + 5.6) (y + 16.0) + rest
               | _ -> dotTemplate (x + 5.6) (y + 16.0) + dotTemplate (x + 7.3) (y + 16.0) + rest
            | R(X32,n) ->
               let yCoord = y + 7.0
               let rest = string x + " " + string yCoord + " 32ndRest "
               match n with
               | 0 -> rest
               | _ -> dotTemplate (x + 6.2) (y + 20.0) + rest
            | R(X64,n) ->
               let yCoord = y + 3.0
               string x + " " + string yCoord + " 64thRest "
            | _ -> ""
         let newX = x + (head.Width * insideScale)
         let newList = l @ [newText]
         // update the element with its x y coords
         let newElement = { head with Location = (x,y) }
         let newUpdatedElements = updatedElements @ [newElement]
         showElements tail newUpdatedElements measureWidth newX y newList insideScale




(* Show all measures of a line
1) measures is the list of SingleMeasures to be evaluated and printed
2) updatedMeasures are the measures with the new elements that have new Locations, to be used for beaming
2) x is the x-coord of the next note
3) y is the y-coord of the next note
4) l is the list of strings to be printed
5) scale is the scale of the measures - width of line / width of measures in the line
RETURNS: list of strings to be printed, and list of updated measures that have th new elements
*)
let rec showMeasures (measures: SingleMeasure List) (updatedMeasures: SingleMeasure List) (x: float) (y: float) (l: string List) (scale: float) : (string List * SingleMeasure List) option =
   match measures with
   | [] -> Some(l,updatedMeasures)
   | head::tail ->
      // list of elements
      let els = head.Elements
      // new Width of the measure based on the scale
      let newWidth = head.Width * scale
      // used to scale the notes on the inside, removing the 5 units of space in the beginning
      // TODO : add -5.0 after newWidth if the last element has a width of less than 15
      let insideScale = newWidth / (head.Width - 5.0)
      match (showElements els [] newWidth x y l insideScale) with
      | Some(li,updatedElements) ->
         // x coordinate of the beginning of the next measure
         let newX = x + newWidth
         // Update the measure with the new elements
         let newMeasure = { head with Elements = updatedElements }
         // Call the beam function, and return a new list of strings that describe how to draw the beams for that new measure
         let listWithBeams = beam newMeasure.Elements li (0.0,0.0) Other 0.0 newMeasure.Time 0 Other
         let newUpdatedMeasures = updatedMeasures @ [newMeasure]
         showMeasures tail newUpdatedMeasures newX y listWithBeams scale
      | None -> None



(* Show all lines of one page
1) lines is the list of Lines to be evaluated and printed
2) updatedLines is the list of new lines with the new measures and elements
3) text is all the postscript text to be written
RETURNS: new updated text and new Line List
*)
let rec showLines (lines: Line List) (updatedLines: Line List) (text: string) : (string * Line List) option =
   match lines with
   // Base case: return the text when all lines have been processed
   | [] -> Some(text,updatedLines)
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
      match (showMeasures newHead.Measures [] newX staffy [] scale) with
      | Some(li,updatedMeasures) ->
         // Put all the strings together
         let allNewElements = staffline + (List.fold (fun acc elem -> acc + " " + elem) "" li)
         // Add the measure number of the first measure of the line
         // Find the measure number
         let firstMeasureNumber = updatedMeasures.Head.MeasureNumber
         // Add the string
         let measureNumberString = string (staffx - 5.0) + " " + string (staffy + 40.0) + " (" + string firstMeasureNumber + ") measureNumber "
         let newText = text + clef + timeSig + allNewElements + measureNumberString
         // Update the line with the new measures
         let newLine = { head with Measures = updatedMeasures }
         let newUpdatedLines = updatedLines @ [newLine]
         showLines tail newUpdatedLines newText
      | None -> None



(* Driver for creating text for postscript file. Every method from here appends onto the base text, which is all the functions and other variables needed
1) pages is list of Pages to be evaluated
2) updatedPages is the list of new pages that have been updated
3) text is the text that will be updated then printed to postscript file
RETURNS string to be printed and the new Page List
*)
let rec show (pages: Page List) (updatedPages: Page List) (text: string) : (string * Page List) option =
   match pages with
   // Base: no more pages, print the text to a file called score.ps
   | [] ->
      File.WriteAllText("score.ps",text)
      Some(text,updatedPages)
   // Recursive case
   | head::tail ->
      let lines = head.Lines
      // Show the lines of a page
      match (showLines lines [] text) with
      | Some(t,updatedLines) ->
         let newText = t + " showpage "
         // update the Page with the new lines
         let newPage = { head with Lines = updatedLines }
         let newUpdatedPages = updatedPages @ [newPage]
         show tail newUpdatedPages newText
      | None -> None




// ********************* DRIVER **************************
let eval ast =
   //decompose
   let (optionsList,measuresList) = ast
   //default options
   let optionsR = {Type = "tab"; Time = (4,4); Key = "c"; Capo = 0; Title = "untitled"; Composer = "unknown"}
   // First, parse the options
   match (evalOption optionsList optionsR) with
   // If the options are valid, parse the measures
   | Some(newOption) ->
      // create SingleMeasure List
      match (evalAllMeasures measuresList newOption []) with
      | Some(list) ->
         // Take SingleMeasure List and use the widths to create list of lines
         //495 is the width of the first line. The rest are 515.
         match (divideLines list [] 495.0) with
         | Some(lines) ->
            // Take Line List and use heights and type to divide into pages
            match (dividePages lines [] (70.0,720.0)) with
            | Some(pages) ->
               //printfn "%A" pages
               let text = "%!PS
               %%BeginProlog
               /concatenate { dup length 2 index length add 1 index type /arraytype eq {array}{string} ifelse dup 0 4 index putinterval dup 4 -1 roll length 4 -1 roll putinterval } bind def
               /printimage { 8 dict begin /color exch def /pathtofile exch def /sizey exch def /sizex exch def /scaley exch def /scalex exch def /ycoord exch def /xcoord exch def gsave xcoord ycoord translate scalex scaley scale sizex sizey 8 [sizex 0 0 -1 sizey mul 0 sizey] pathtofile (r) file /DCTDecode filter false color colorimage grestore end } bind def
               /timesignature { 7 dict begin /num exch def /ycoord exch def /xcoord exch def /str (images/Time_Signature/0.jpg) def /num2 {num 48 add} bind def num 10 ge { /str (images/Time_Signature/10.jpg) store /tens num 10 idiv 48 add def /ones num 10 mod 48 add def str 22 tens put str 23 ones put xcoord 3 sub ycoord 13.2575758 8.909 125 84 str 3 printimage }{str 22 num2 put xcoord ycoord 7 8.909 66 84 str 3 printimage } ifelse } bind def
               /staffline { 4 dict begin /first exch def /ycoord exch def /xcoord exch def /width 515 def first 1 eq {/width 495 store} {} ifelse xcoord ycoord moveto 0.4 setlinewidth width 0 rlineto stroke end } bind def
               /barline { 4 dict begin /linewidth exch def /height exch def /ycoord exch def /ycoord ycoord 0.2 sub store /xcoord exch def gsave linewidth setlinewidth xcoord ycoord moveto 0 height rlineto stroke grestore end } bind def
               /guitartablines { 5 dict begin /flag exch def /ycoord exch def /xcoord exch def gsave 1.33 setlinewidth xcoord ycoord 30.4 1.33 barline stroke 0.4 setlinewidth 0 1 5 { /num exch def xcoord num 6 mul ycoord add flag staffline } for 1.33 setlinewidth /width 515 def flag 1 eq {/width 495 store}{} ifelse xcoord width add ycoord 30.4 1.33 barline stroke xcoord ycoord 40 fancyline end } bind def
               /fancyline { 3 dict begin /height exch def /ycoord exch def /xcoord exch   def xcoord 5 sub ycoord 5 sub moveto 2.5 setlinewidth 0 height rlineto stroke newpath 0.1 setlinewidth xcoord 4 sub ycoord 5 sub moveto xcoord 2 sub ycoord 5 sub xcoord 0.5 sub ycoord 5.5 sub xcoord 2 add ycoord 8 sub curveto xcoord ycoord 4.666 sub xcoord 6 sub ycoord 3 sub 10 arct closepath fill newpath xcoord 4 sub ycoord 5 sub height add moveto xcoord 2 sub ycoord 5 sub height add xcoord 0.5 sub ycoord 4.5 sub height add xcoord 2 add ycoord 2 sub height add curveto xcoord ycoord 5.333 sub height add xcoord 6 sub ycoord 7 sub height add 10 arct closepath fill } bind def
               /guitarfretnumber { 8 dict begin /str exch def /ycoord exch def /xcoord exch def /scalex 4 def /scaley 4.51 def /sizex 800 def /sizey 902 def /filestring (temp) def str type /stringtype eq { /xcoord xcoord 0.4 sub store /filestring (images/Tab_Numbers/) str (.jpg) concatenate concatenate store /scalex 4.6 store /scaley 4.8 store /sizex 1000 store }{ str 9 gt { /xcoord xcoord 1.7 sub store /scalex 7.3 store /sizex 1460 store }{} ifelse /filestring (images/Tab_Numbers/) str (ffff) cvs (.jpg) concatenate concatenate store } ifelse xcoord ycoord scalex scaley sizex sizey filestring 1 printimage end } bind def
               /quarterRest { 2 dict begin gsave 0.1 setlinewidth /ycoord exch def /xcoord exch def /ycoord ycoord 12 add store xcoord ycoord moveto xcoord 2.8000000000000003 add ycoord 3.857142857142857 sub lineto xcoord 0.5714285714285714 add ycoord 6.571428571428571 sub xcoord 1.1428571428571428 add ycoord 7.285714285714286 sub xcoord 3.4285714285714284 add ycoord 9.514285714285714 sub curveto xcoord 0.8571428571428571 add ycoord 8.657142857142857 sub xcoord ycoord 10.085714285714285 sub xcoord 1.657142857142857 add ycoord 12.085714285714285 sub curveto xcoord 1.657142857142857 add ycoord 12.142857142857142 sub xcoord 1.5714285714285714 add ycoord 12.200000000000001 sub xcoord 1.4857142857142858 add ycoord 12.12857142857143 sub curveto xcoord 2.142857142857143 sub ycoord 10.0 sub xcoord 0.19999999999999998 add ycoord 7.428571428571429 sub xcoord 1.7142857142857142 add ycoord 8.285714285714286 sub curveto xcoord 0.6571428571428571 sub ycoord 5.257142857142857 sub lineto xcoord 1.1428571428571428 add ycoord 3.257142857142857 sub xcoord 1.1428571428571428 add ycoord 2.2857142857142856 sub xcoord 0.24285714285714285 sub ycoord 0.19999999999999998 sub curveto xcoord 0.24285714285714285 sub ycoord 0.1142857142857143 sub xcoord 0.14285714285714285 sub ycoord xcoord ycoord curveto fill grestore end } bind def
               /restCurl { 2 dict begin gsave 0.1 setlinewidth /ycoord exch def /xcoord exch def newpath xcoord ycoord moveto xcoord 0.64816513755 sub ycoord 0.51853211004 sub xcoord 1.058669724665 sub ycoord 0.885825687985 sub xcoord 1.46917431178 sub ycoord 0.99385321091 sub curveto xcoord 2.46302752269 sub ycoord 1.85807339431 sub lineto xcoord 2.03091743099 sub ycoord 1.85807339431 sub xcoord 1.85807339431 sub ycoord 1.77165137597 sub xcoord 0.95064220174 sub ycoord 1.4475688071950001 sub curveto xcoord 0.8642201834000001 sub ycoord 1.42596330261 sub xcoord 0.8642201834000001 sub ycoord 1.663623853045 sub 0.08642201834 arct closepath fill newpath xcoord 2.46302752269 sub ycoord 0.8210091742300001 sub 1.03706422008 0 360 arc fill grestore end } bind def
               /8thRest { 2 dict begin gsave 0.1 setlinewidth /ycoord exch def /xcoord exch def /xcoord xcoord 1 add store xcoord ycoord moveto xcoord 0.04321100917 add ycoord 0.194449541265 sub xcoord 0.8210091742300001 add ycoord 0.15123853209500002 sub xcoord 0.99385321091 add ycoord curveto xcoord 2.72229357771 add ycoord 6.22238532048 add lineto xcoord 2.72229357771 add ycoord 6.3520183479900005 add xcoord 2.5062385318600002 add ycoord 6.5248623846700005 add xcoord 2.37660550435 add ycoord 6.43844036633 add curveto xcoord 1.51238532095 add ycoord 4.904449540795 add lineto xcoord 1.51238532095 add ycoord 4.58036697202 add xcoord 0.04321100917 sub ycoord 0.12963302751 add xcoord 0.04321100917 sub ycoord 0.12963302751 add curveto xcoord 0.08642201834 sub ycoord xcoord 0.051853211004 add ycoord 0.064816513755 sub 0.17284403668 arct fill xcoord 2.37660550435 add ycoord 6.43844036633 add restCurl grestore end } bind def
               /16thRest { 2 dict begin gsave 0.1 setlinewidth /ycoord exch def /xcoord exch def xcoord ycoord moveto xcoord 0.04321100917 add ycoord 0.194449541265 sub xcoord 0.8210091742300001 add ycoord 0.15123853209500002 sub xcoord 0.99385321091 add ycoord curveto xcoord 3.9322018344700003 add ycoord 10.15458715495 add lineto xcoord 3.9538073390550004 add ycoord 10.262614677875 add xcoord 3.7377522932050002 add ycoord 10.435458714555 add xcoord 3.62972477028 add ycoord 10.3706422008 add curveto xcoord 2.76550458688 add ycoord 8.836651375265001 add lineto xcoord 1.51238532095 add ycoord 4.58036697202 add xcoord 0.04321100917 sub ycoord 0.12963302751 add xcoord 0.04321100917 sub ycoord 0.12963302751 add curveto xcoord 0.08642201834 sub ycoord xcoord 0.051853211004 add ycoord 0.064816513755 sub 0.17284403668 arct fill xcoord 2.4198165135200003 add ycoord 6.43844036633 add restCurl xcoord 3.62972477028 add ycoord 10.3706422008 add restCurl grestore end } bind def
               /32ndRest { 2 dict begin gsave 0.1 setlinewidth /ycoord exch def /xcoord exch def xcoord ycoord moveto xcoord 0.04321100917 add ycoord 0.194449541265 sub xcoord 0.8210091742300001 add ycoord 0.15123853209500002 sub xcoord 0.99385321091 add ycoord curveto xcoord 4.515550458265 add ycoord 14.151605503175 add lineto xcoord 4.53715596285 add ycoord 14.17321100776 add xcoord 4.429128439925 add ycoord 14.367660549025 add xcoord 4.23467889866 add ycoord 14.272596328851002 add curveto xcoord 3.41366972443 add ycoord 12.79045871432 add lineto xcoord 1.33954128427 add ycoord 4.58036697202 add xcoord 0.04321100917 sub ycoord 0.12963302751 add xcoord 0.04321100917 sub ycoord 0.12963302751 add curveto xcoord 0.08642201834 sub ycoord xcoord 0.051853211004 add ycoord 0.064816513755 sub 0.17284403668 arct fill xcoord 2.20376146767 add ycoord 6.43844036633 add restCurl xcoord 3.2624311923350002 add ycoord 10.3706422008 add restCurl xcoord 4.277889907830001 add ycoord 14.30284403527 add restCurl grestore end } bind def
               /64thRest { 2 dict begin gsave 0.1 setlinewidth /ycoord exch def /xcoord exch def xcoord ycoord moveto xcoord 0.04321100917 add ycoord 0.194449541265 sub xcoord 0.8210091742300001 add ycoord 0.15123853209500002 sub xcoord 0.99385321091 add ycoord curveto xcoord 5.53100917376 add ycoord 18.1486238514 add lineto xcoord 5.48779816459 add ycoord 18.27825687891 add xcoord 5.44458715542 add ycoord 18.32146788808 add xcoord 5.250137614155 add ycoord 18.269614677076 add curveto xcoord 4.40752293534 add ycoord 16.76587155796 add lineto xcoord 1.33954128427 add ycoord 4.58036697202 add xcoord 0.04321100917 sub ycoord 0.12963302751 add xcoord 0.04321100917 sub ycoord 0.12963302751 add curveto xcoord 0.08642201834 sub ycoord xcoord 0.051853211004 add ycoord 0.064816513755 sub 0.17284403668 arct fill xcoord 2.20376146767 add ycoord 6.43844036633 add restCurl xcoord 3.24082568775 add ycoord 10.3706422008 add restCurl xcoord 4.277889907830001 add ycoord 14.30284403527 add restCurl xcoord 5.27174311874 add ycoord 18.27825687891 add restCurl grestore end } bind def
               /halfWholeRest { 2 dict begin gsave 0.1 setlinewidth /ycoord exch def /xcoord exch def xcoord 0.37129898 add ycoord moveto xcoord 4.48414922 add ycoord xcoord 4.48414922 add ycoord 0.37129898 add 0.37129898 arct xcoord 4.48414922 add ycoord 2.31347826 add xcoord 4.11285024 add ycoord 2.31347826 add 0.37129898 arct xcoord ycoord 2.31347826 add xcoord ycoord 0.37129898 add 0.37129898 arct xcoord ycoord xcoord 0.37129898 add ycoord 0.37129898 arct fill grestore end } bind def
               /drawFlag { 2 dict begin gsave /ycoord exch def /xcoord exch def 0.1 setlinewidth xcoord ycoord moveto xcoord ycoord 2.744186046511628 add lineto xcoord 0.20930232558139536 add ycoord 2.697674418604651 add 0.20930232558139536 170 10 arcn xcoord 0.6976744186046512 add ycoord 1.627906976744186 sub xcoord 4.3023255813953485 add ycoord 1.627906976744186 sub xcoord 2.744186046511628 add ycoord 7.1395348837209305 sub curveto xcoord 2.604651162790698 add ycoord 7.372093023255814 sub xcoord 2.2325581395348837 add ycoord 7.325581395348837 sub xcoord 2.2093023255813953 add ycoord 6.976744186046512 sub curveto xcoord 3.13953488372093 add ycoord 3.953488372093023 sub xcoord 2.488372093023256 add ycoord 2.7906976744186047 sub xcoord ycoord curveto fill grestore end } bind def
               /measureNumber { 3 dict begin gsave /str exch def /ycoord exch def /xcoord exch def /Times-Roman findfont 7 scalefont setfont newpath 0 0 0 setrgbcolor xcoord ycoord moveto str show grestore end } bind def
               /centerText { dup stringwidth pop -0.5 mul 0 rmoveto show } def
               /title { 1 dict begin gsave /str exch def /Times-Roman findfont 22 scalefont setfont newpath 0 0 0 setrgbcolor 306 745 moveto str centerText grestore end } bind def
               /composer { 1 dict begin gsave /str exch def /Times-Roman findfont 10 scalefont setfont newpath 0 0 0 setrgbcolor 565 720 moveto str stringwidth pop -1 mul 0 rmoveto str show grestore end } bind def
               %%EndProlog
               "
               // Add the title and comnposer to the text
               let text' = text + " (" + newOption.Title + ") title " + "(" + newOption.Composer + ") composer "
               //print and show
               match (show pages [] text') with
               | Some(updatedText, updatedPages) ->

                  Some(updatedText, updatedPages)
               | None -> None
            | None -> None
         | None -> None
      | None -> None
   | None ->None
































///
