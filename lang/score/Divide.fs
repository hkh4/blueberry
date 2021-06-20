module Divide

open System
open Types
open Options

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
7) graceNotes is the list of grace notes for this element
RETURNS: new Element and the new nextStart for the next element
*)
let widthStart (template: Element) (r: Rhythm) (nextStart: float) (baseBeat: RhythmNumber) (numberOfBeats: int) (last: int) (graceNotes: Element List) : (Element * float) option =
   // Find the index of the baseBeat in the list of rhythms
   let indexOfBeat = Array.findIndex (fun elem -> elem = baseBeat) arrayOfRhythms
   let (rNumber,dotNumber) =
      match r with
      | R(a,b) -> a,b
      | Other ->
         printfn "Error in widthStart. The note had a rhythm of type Other"
         X4,0 // Should never reach this

   match rNumber with
   // If it's X0, which should be a rest, its number of beats is the whole measure
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

         match template.Width with
         // if the width isn't set to 0, then it must be a tuplet note. Don't do any adjusting
         | num when num <> 0.0 -> template.Width
         | _ ->
            match last with
            // If this isn't the last note, return
            | 0 -> newWidthTemp
            | _ ->
               // If it is the last note, and the width is less than 10, set it to 10 so that there's sufficient space before the next bar line
               match newWidthTemp with
               | num when num < 13.0 -> 13.0
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

      // helper function to add up the widths of all the grace notes, and also update the widths of those grace notes. Returns the total width and the new list of grace notes with updated widths
      let rec graceWidthHelper (graceToAdd: Element List) (newGrace: Element List) (acc: float) : float * Element List =
         match graceToAdd with
         | [] -> (acc,newGrace)
         | head::tail ->
            let graceWidth = widthOfGraceRhythms.[head.Duration]
            let newHead = { head with Width = graceWidth }
            let newList = newGrace @ [newHead]
            graceWidthHelper tail newList (acc + graceWidth)


      // If the note has grace notes, increase its width some more
      match graceNotes with
      // no grace notes
      | [] ->

         // Check if it's a tupletNote; if so, check for grace notes in the first note of the tuplet
         match template.NoteInfo with
         | TupletNote(nList) ->

            // Get the first note
            let firstNote = nList.Head

            // See if it has grace notes; if so, call the helper and return the new note and the additional width
            let (newFirstNote, extraWidth) =
               match firstNote.GraceNotes with
               | [] -> (firstNote,0.0)
               | gNotes ->
                  // call the helper
                  let (extra, newGrace) = graceWidthHelper gNotes [] 0.0
                  // update the width of this note
                  let widthWithGrace = firstNote.Width + extra
                  let updatedFirstNote = { firstNote with Width = widthWithGrace; GraceNotes = newGrace }
                  (updatedFirstNote, extra)

            // update the width of the entire tuplet
            let newTupletWidth = template.Width + extraWidth

            // new tuplet notes
            let newTupletNotes = newFirstNote::nList.Tail
            let newNInfo = TupletNote(newTupletNotes)

            Some({ template with Width = newTupletWidth; NoteInfo = newNInfo }, newNextStart)

         // not a tupletNote - do nothing
         | _ ->
            // Return the new element with updated width, and the next start
            Some({ template with Width = newWidth },newNextStart)
      // has grace notes

      | graceList ->

         // call the helper
         let (extra,newGrace) = graceWidthHelper graceList [] 0.0
         // add this extra width to the existing width of the actual note
         let widthWithGrace = newWidth + extra
         // modify the note to have the larger width and the updated grace notes
         Some({ template with Width = widthWithGrace; GraceNotes = newGrace },newNextStart)





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





(* Parse a simple note
1) p is the simple note to be parsed
2) baseBeat is the bottom number of the time signature
3) numberOfBeats is the top note of the time signature
4) nextStart is the next start value for this note
5) last is whether or not this is the last note of the measure (1 yes 0 no)
6) optionsR is the record of options
7) graceBefore is the list of grace notes for this note
8) measureNumber is the measure number
RETURNS the new element and a bool that says whether or not this is a grace note
*)
let parseSimple (p: simple) (baseBeat: RhythmNumber) (numberOfBeats: int) (nextStart: float) (last: int) (optionsR: optionsRecord) (graceBefore: Element List) (measureNumber: int) : (Element * bool) option =

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
            SingleNote(NormalGuitarNote(guitarString,pitch,0,eProperties),mProperties)
      // Check to see if it's a grace note
      match (List.exists (fun e -> e = Gra) mProperties) with
      // not a grace note
      | false ->
         let graceBeforeBuffer =
            match graceBefore with
            | [] -> graceBefore
            | _ -> graceBefore @ [bufferElement]
         Some(({ NoteInfo = nInfo; Start = nextStart; Comments = ""; Duration = defaultRhythm; Width = 0.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = graceBeforeBuffer }),false)
      // grace note
      | true ->
         Some(({ NoteInfo = nInfo; Start = nextStart; Comments = ""; Duration = defaultRhythm; Width = 0.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = [] }),true)
   // Rest Simple
   | RestSimple ->
      // rests can't have grace notes
      match graceBefore with
      | [] ->
         Some({ NoteInfo = Rest; Start = nextStart; Comments = ""; Duration = defaultRhythm; Width = 0.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = [] },false)
      | _ ->
         printfn "Error in measure %i! Rests can't have grace notes!" measureNumber
         None




(* Parse a complex note
1) p is the complex note to be parsed
2) baseBeat is the bottom number of the time signature
3) numberOfBeats is the top note of the time signature
4) nextStart is the next start value for this note
5) last is whether or not this is the last note of the measure (1 yes 0 no)
6) optionsR is the record of options
7) graceBefore is the list of grace notes for this note
8) measureNumber is the measure number
RETURNS the new element and a bool that says whether or not this is a grace note
*)
let parseComplex (p: complex) (baseBeat: RhythmNumber) (numberOfBeats: int) (nextStart: float) (last: int) (optionsR: optionsRecord) (graceBefore: Element List) (measureNumber: int) : (Element * bool) option =

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
            SingleNote(NormalGuitarNote(guitarString,pitch,0,eProperties),mProperties)
      defaultRhythm <- r
      match (List.exists (fun e -> e = Gra) mProperties) with
      // not a grace note
      | false ->
         let graceBeforeBuffer =
            match graceBefore with
            | [] -> graceBefore
            | _ -> graceBefore @ [bufferElement]
         Some(({ NoteInfo = nInfo; Start = nextStart; Comments = ""; Duration = r; Width = 0.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = graceBeforeBuffer }),false)
      // grace note
      | true ->
         Some(({ NoteInfo = nInfo; Comments = ""; Start = nextStart; Duration = r; Width = 0.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = [] }),true)
   // Rest Complex
   | RestComplex(r) ->
      match graceBefore with
      | [] ->
         // Only update default rhythm if the rhythm is NOT X0
         match r with
         | R(X0,0) ->
            Some({ NoteInfo = Rest; Start = nextStart; Comments = ""; Duration = r; Width = 0.0; LastNote = 0; Location = (0.0,0.0);  GraceNotes = [] },false)
         | _ ->
            defaultRhythm <- r
            Some({ NoteInfo = Rest; Start = nextStart; Comments = ""; Duration = r; Width = 0.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = [] },false)
      | _ ->
         printfn "Error in measure %i! Rests can't have grace notes!" measureNumber
         None




(* Parse a group note
1) p is the group note to be parsed
2) baseBeat is the bottom number of the time signature
3) numberOfBeats is the top note of the time signature
4) nextStart is the next start value for this note
5) last is whether or not this is the last note of the measure (1 yes 0 no)
6) optionsR is the record of options
7) graceBefore is the list of grace notes for this note
8) measureNumber is the measure number
RETURNS the new element and a bool that says whether or not this is a grace note
*)
let parseGroup (g: group) (baseBeat: RhythmNumber) (numberOfBeats: int) (nextStart: float) (last: int) (optionsR: optionsRecord) (graceBefore: Element List) (measureNumber: int) : (Element * bool) option =

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
               printfn "Error in measure %i! You can't specify two notes in one group that are on the same string!" measureNumber
               None
            | false ->
               let newSingleNote =
                  match pitch with
                  // X note
                  | NoPitch -> X(guitarString,eProperties)
                  // regular note
                  | _ -> NormalGuitarNote(guitarString,pitch,0,eProperties)
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
            let graceBeforeBuffer =
               match graceBefore with
               | [] -> graceBefore
               | _ -> graceBefore @ [bufferElement]
            Some({ NoteInfo = newGroup; Start = nextStart; Comments = ""; Duration = defaultRhythm; Width = 0.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = graceBeforeBuffer },false)
         // grace note
         | true ->
            Some({ NoteInfo = newGroup; Start = nextStart; Comments = ""; Duration = defaultRhythm; Width = 0.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = [] },true)
      | None -> None
   // gcomplex: group with rhythm: does the same thing but uses r instead of the default rhythm
   | GComplex(gList,r,mProperties) ->
      defaultRhythm <- r
      match (groupHelper gList [] []) with
      | Some(singleNoteList) ->
         // turn it into a Group type
         let newGroup = GroupNote(singleNoteList,mProperties)
         match (List.exists (fun e -> e = Gra) mProperties) with
         // not a grace note
         | false ->
            let graceBeforeBuffer =
               match graceBefore with
               | [] -> graceBefore
               | _ -> graceBefore @ [bufferElement]
            Some({ NoteInfo = newGroup; Start = nextStart; Comments = ""; Duration = r; Width = 0.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = graceBeforeBuffer },false)
         // grace note
         | true ->
            Some({ NoteInfo = newGroup; Start = nextStart; Comments = ""; Duration = r; Width = 0.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = [] },true)
      | None -> None




(* Parse a tuplet note
1) p is the tuplet note to be parsed
2) baseBeat is the bottom number of the time signature
3) numberOfBeats is the top note of the time signature
4) nextStart is the next start value for this note
5) last is whether or not this is the last note of the measure (1 yes 0 no)
6) optionsR is the record of options
7) graceBefore is the list of grace notes for this note
8) measureNumber is the measure number
RETURNS the new element and a bool that says whether or not this is a grace note
*)
let parseTuplet (t: Note List) (r: Rhythm) (baseBeat: RhythmNumber) (numberOfBeats: int) (nextStart: float) (last: int) (optionsR: optionsRecord) (graceBefore: Element List) (measureNumber: int) : (Element * bool) option =

   // recursive helper to parse each note within the tuplet
   let rec tHelper (notes: Note List) (newElements: Element List) (totalWidth: float) : (Element * bool) option =

      match notes with
      | [] ->
         // turn the list of Elements into a single Element

         let bigElement = { NoteInfo = TupletNote(newElements); Comments = ""; Start = nextStart; Duration = r; Width = totalWidth; LastNote = 0; Location = (0.0,0.0); GraceNotes = [] }

         //update default rhythm
         defaultRhythm <- r

         Some(bigElement,false)

      | head::tail ->

         //parse each individual note
         let noteOption : (Element * bool) option =
            match head with

            | Simple(p) ->
               parseSimple p baseBeat numberOfBeats nextStart last optionsR graceBefore measureNumber

            | Complex(p) ->
               parseComplex p baseBeat numberOfBeats nextStart last optionsR graceBefore measureNumber

            | Group(g) ->
               parseGroup g baseBeat numberOfBeats nextStart last optionsR graceBefore measureNumber

            | Tuplet(t,r) ->
               printfn "Error in measure %i! Can't have a tuplet within a tuplet" measureNumber
               None

            | _ ->
               printfn "Error in measure %i! Can't have a comment within a tuplet" measureNumber
               None

         match noteOption with

         // will never be a grace note
         | Some(note,b) ->
            // Check to see if a note has a valid number of dots. 8th notes and longer can up to 3 dots. 16th can have 2, 32nd can have 1, 64th cannot have any
            match note.Duration with
            | R(x,n) when n > 3 ->
               printfn "Error in measure %i! Notes cannot have more than 3 dots" measureNumber
               None
            | R(x,n) when x = X0 && n > 0 ->
               printfn "Error in measure %i! 0 rhythms cannot have dots" measureNumber
               None
            | R(x,n) when x = X64 && n > 0 ->
               printfn "Error in measure %i! 64th notes cannot have any dots" measureNumber
               None
            | R(x,n) when x = X32 && n > 1 ->
               printfn "Error in measure %i! 32nd notes can only have up to 1 dot" measureNumber
               None
            | R(x,n) when x = X16 && n > 2 ->
               printfn "Error in measure %i! 16th notes can only have up to 2 dots" measureNumber
               None
            | _ ->
               // Don't care about the start for tuplet notes
               // Look into the Map of rhythms to widths
               // Should be smaller for tuplet notes, so divide by 1.2
               let newWidthTemp = widthOfRhythms.[note.Duration] / 1.5

               let newWidth =
                  match last with
                  // If this isn't the last note, return
                  | 0 -> newWidthTemp
                  | _ ->
                     // If it is the last note, and the width is less than 10, set it to 10 so that there's sufficient space before the next bar line
                     // But, since it's a tuplet, only do this for the last note of the tuplet
                     match tail with
                     | head::tail -> newWidthTemp
                     | [] ->
                        match newWidthTemp with
                        | num when num < 13.0 -> 13.0
                        | _ -> newWidthTemp

               // add grace notes to the first note
               let newNote =
                  match newElements with
                  // empty, this is the first note
                  | [] ->

                     // the grace notes should be placed into the first note
                     let graceBeforeBuffer =
                        match graceBefore with
                        | [] -> graceBefore
                        | _ -> graceBefore @ [bufferElement]
                     { note with Width = newWidth; GraceNotes = graceBeforeBuffer }
                  | _ ->
                     { note with Width = newWidth; GraceNotes = [] }

               tHelper tail (newElements @ [newNote]) (totalWidth + newWidth)

         | None -> None


   tHelper t [] 0.0




(* Evaluate a single note
1) measureNumber is the number of the current measure
2) n is the current Note
3) baseBeat is what rhythm counts as one beat, based on bottom number of time signature
4) numberOfBeats is top number of time signature - number of beats in a measure
5) nextStart is the starting spot of the next note
6) last is either 1, meaning it's the last note, or 0 otherwise
7) optionsR is the optionsRecord
8) graceBefore is the list of grace notes that precede this note. If this note isn't a grace note, then this list is added to this element. Otherwise, add this note to the list and return it
RETURNS: an Element, a float which is the start of the next note, and the list of grace notes
*)
let evalNote (measureNumber: int) (n: Note) (baseBeat: RhythmNumber) (numberOfBeats: int) (nextStart: float) (last: int) (optionsR: optionsRecord) (graceBefore: Element List) : (Element * float * Element List) option =
   // this is EITHER a tuple of an Element and a bool (true means grace note, else not) or None

   let noteOption =
      match n with // figure out the type of the note

      | Simple(p) ->
         parseSimple p baseBeat numberOfBeats nextStart last optionsR graceBefore measureNumber

      | Complex(p) ->
         parseComplex p baseBeat numberOfBeats nextStart last optionsR graceBefore measureNumber

      | Group(g) ->
         parseGroup g baseBeat numberOfBeats nextStart last optionsR graceBefore measureNumber

      | Tuplet(t,r) ->
         parseTuplet t r baseBeat numberOfBeats nextStart last optionsR graceBefore measureNumber

      // If it's a comment, just give an empty element and take care of it later
      | Comment(s) ->
         Some({emptyElement with Comments = s}, false)

      // should never reach this case
      | HiddenComment ->
         printfn "Error in evalNote: HiddenComment given"
         None



   match noteOption with
   // if it's not a grace note
   | Some(note,b) when b = false ->

      // Check to see if a note has a valid number of dots. 8th notes and longer can up to 3 dots. 16th can have 2, 32nd can have 1, 64th cannot have any
      match note.Duration with
      | R(x,n) when n > 3 ->
         printfn "Error in measure %i! Notes cannot have more than 3 dots" measureNumber
         None
      | R(x,n) when x = X0 && n > 0 ->
         printfn "Error in measure %i! 0 rhythms cannot have dots" measureNumber
         None
      | R(x,n) when x = X64 && n > 0 ->
         printfn "Error in measure %i! 64th notes cannot have any dots" measureNumber
         None
      | R(x,n) when x = X32 && n > 1 ->
         printfn "Error in measure %i! 32nd notes can only have up to 1 dot" measureNumber
         None
      | R(x,n) when x = X16 && n > 2 ->
         printfn "Error in measure %i! 16th notes can only have up to 2 dots" measureNumber
         None

      // If it's Other, that means it is a comment.
      | Other ->
         Some(note, nextStart, graceBefore)

      | _ ->

         // Call widthStart to create the note element object with updated width
         match (widthStart note (note.Duration) nextStart baseBeat numberOfBeats last note.GraceNotes) with
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

                  // If it's measure 0, it's fine, because it's a pick up measure
                  match measureNumber with
                  | 0 ->
                     Some({ newNote with LastNote = 1 },newNextStart,[])
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
               | _ -> Some({ newNote with LastNote = 0 },newNextStart,[])
         | None -> None
   | Some(note,b) ->
      Some(note,nextStart,(graceBefore @ [note]))
   | None -> None





(* Recursive helper for measure evaluator, calls the note evaluator and composes the list of Elements, returns a SingleMeasure
1) measureNumber is the number of the current measure
2) m is the list of Notes remaining to be evaluated, with hiddenComments removed
3) noComments is the list with all comments and hiddencomments removed
4) elementList is the list of Elements which is built upon recursively by calling evalNote
5) baseBeat is the bottom number of time signature
6) numberOfBeats is the top number of time signature
7) acc is the accumulator to keep track of the total width of all the elements in the measure
8) nextStart is the start of the next element
9) optionsR is the optionsRecord
10) graceBefore is the list of notes that are grace notes for the next element
RETURNS: float which is the total width of the measure, and the list of elements that make up the measure
*)
let rec evalMeasureHelper (measureNumber: int) (m : Note List) (noComments : Note List) (elementList : Element List) (baseBeat: RhythmNumber) (numberOfBeats: int) (acc : float) (nextStart: float)  (optionsR: optionsRecord) (graceBefore: Element List) : (float * Element List) option =
   match m with
   | [] -> Some(acc, elementList)
   | head::tail ->

      let (el, newNoComments)=
         match head with

         // If it's a comment, the noComments list stays the same
         | Comment(s) ->
            let elTemp = evalNote measureNumber head baseBeat numberOfBeats nextStart 0 optionsR graceBefore
            (elTemp, noComments)

         // If it's a note, check if it's the last note
         | _ ->

            let elTemp =
               // if tail of the list without comments is empty, then this note is the last one
               match noComments.Tail with
               | [] -> evalNote measureNumber head baseBeat numberOfBeats nextStart 1 optionsR graceBefore
               | _ -> evalNote measureNumber head baseBeat numberOfBeats nextStart 0 optionsR graceBefore

            (elTemp, noComments.Tail)

      match el with
      | Some(n,newNextStart,newGraceBefore) ->

         match n with

         // If it wasn't a comment
         | {Element.Comments = c} when c = "" ->

            match newGraceBefore with
            // if it's empty, then the returned note was NOT a grace note
            | [] ->
               // keep track of the total width of the measure
               let newAcc = n.Width + acc
               // append new element to the end of the list
               let newList = elementList @ [n]
               evalMeasureHelper measureNumber tail newNoComments newList baseBeat numberOfBeats newAcc newNextStart optionsR newGraceBefore
            | _ ->
               evalMeasureHelper measureNumber tail newNoComments elementList baseBeat numberOfBeats acc newNextStart optionsR newGraceBefore

         // If it was a comment, add the comment to the last note and recurse
         | {Element.Comments = c} ->

            match elementList with
            | [] ->
               printfn "A comment cannot come before the first note in a measure."
               None
            | l ->
               let rev = List.rev elementList
               let last = rev.Head
               let lastWithComment = { last with Comments = c }
               let newRev = lastWithComment::rev.Tail
               let newElementList = List.rev newRev
               evalMeasureHelper measureNumber tail newNoComments newElementList baseBeat numberOfBeats acc newNextStart optionsR newGraceBefore


      | None -> None





(* Depending on the key, change the pitches
1) l is the list of Elements
2) key is the key of the measure
RETURNS the updated list of elements
*)
let rec parseKey (l: Element List) (key: string) : Element List =
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
         // parse the key of the grace notes
         let updatedGraceNotes =
            match head.GraceNotes with
            | [] -> head.GraceNotes
            | gList -> parseKeyHelper head.GraceNotes [] mapOfChanges
         // update head using the new grace notes
         let newHead = { head with GraceNotes = updatedGraceNotes }

         match newHead.NoteInfo with
         // if it's a single guitar note
         | SingleNote(NormalGuitarNote(s,pitch,fret,eProperties),mProperties) ->
            // see if this pitch needs to be changed
            match mapOfChanges.TryFind pitch with
            // change the pitch
            | Some(newPitch) ->
               // update all the info for the new Element
               let newNInfo = SingleNote(NormalGuitarNote(s,newPitch,fret,eProperties),mProperties)
               let newElement = { newHead with NoteInfo = newNInfo }
               let newList = updatedList @ [newElement]
               parseKeyHelper tail newList mapOfChanges
            // don't change this pitch, recurse
            | None ->
               let newList = updatedList @ [newHead]
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
                  | NormalGuitarNote(guitarString,pitch,fret,eProperties) ->
                     match mapOfChanges.TryFind pitch with
                     | Some(newPitch) ->
                        let newNInfo = NormalGuitarNote(guitarString,newPitch,fret,eProperties)
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
            let newElement = { newHead with NoteInfo = newNInfo }
            // new List, recurse
            let newList = updatedList @ [newElement]
            parseKeyHelper tail newList mapOfChanges
         // for all other types of Notes, no pitch change needed
         | TupletNote(nList) ->

            // First, change the key of all the notes within the tuple
            let newTupletNotes = parseKey nList key

            // Create the new tuple
            let newNInfo = TupletNote(newTupletNotes)
            let newElement = { head with NoteInfo = newNInfo }
            let newList = updatedList @ [newElement]
            parseKeyHelper tail newList mapOfChanges

         | _ ->
            let newList = updatedList @ [newHead]
            parseKeyHelper tail newList mapOfChanges


   // call the helper, return its result
   parseKeyHelper l [] mapOfChanges






(* Evaluate a single measure
1) m is an Expr
2) optionsR is the options evaluated earlier, which will be added into the metadata of the measure
3) changes is an anonymous record of which options have changed
RETURNS: a SingleMeasure
*)
let evalMeasure (m: Expr) (optionsR: optionsRecord) (changes: {| Time: bool; Key: bool; Capo: bool |}) : SingleMeasure option =
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

      // Create two new instances of the note list: one without hiddencomments and one without both comments and hiddencomments
      let rec noCommentsF original (noHidden : Note List) (noComments : Note List) =
         match original with
         | [] -> (noHidden, noComments)
         | head::tail ->
            match head with
            | Comment(s) ->
               noCommentsF tail (noHidden @ [head]) noComments
            | HiddenComment ->
               noCommentsF tail noHidden noComments
            | _ ->
               noCommentsF tail (noHidden @ [head]) (noComments @ [head])


      let (noHidden, noComments) = noCommentsF c [] []

      match (evalMeasureHelper b noHidden noComments [] baseBeat numberOfBeats acc 1.0 optionsR []) with

      | Some(width,list) ->

         // update the notes based on the key
         let listWithUpdatedKeys = parseKey list optionsR.Key

         // Add empty space at the beginning and barline at the end
         let empty = { NoteInfo = Empty; Start = 0.0; Comments = ""; Duration = Other; Width = 5.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = [] }

         // Barline at the end of the measure
         let bar = { NoteInfo = Barline; Start = 0.0; Comments = ""; Duration = Other; Width = 0.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = [] }
         let newList = [empty] @ listWithUpdatedKeys @ [bar]

         // Add 5 to the width because of the empty space at the beginning
         let mutable newWidth = width + 5.0

         // If it needs a time change, add that
         let timeList =
            match changes.Time with
            | true ->
               let (top,bottom) = optionsR.Time
               newWidth <- newWidth + 10.0
               let timeChange = { NoteInfo = TimeChange(top,bottom); Comments = ""; Start = 0.0; Duration = Other; Width = 10.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = [] }
               timeChange::newList

            | false -> newList

         // create instance of SingleMeasure
         let mes = { Time = optionsR.Time; Key = optionsR.Key; Capo = optionsR.Capo; MeasureNumber = b; Elements = timeList; Width = newWidth; Changes = {| Key = changes.Key; Time = changes.Time; Capo = changes.Capo |}}


         Some(mes)
      | None -> None
   | _ ->
      printfn "Something is very wrong. Given ScoreOption instead of Measure"
      None





(* Goes through each measure one at a time, gives to evalMeasure, returns list of SingleMeasure
1) measureList is the list of Expr's
2) optionsR is the record of options evaluated earlier
3) singleMeasureList is a list of SingleMeasures that will be evaluated and appended
4) changes is an anonymous record of which options have changed
RETURNS: list of SingleMeasure evaluated by helpers
*)
let rec evalAllMeasures (measuresList : Expr List) (optionsR : optionsRecord) (singleMeasureList : SingleMeasure List) (changes: {| Time: bool; Key: bool; Capo: bool |}) : SingleMeasure List option =
   match measuresList with
   // Base case : return SingleMeasure List
   | [] ->
      Some(singleMeasureList)
   | head::tail ->

      // If it's a measure, parse it
      match head with
      | Measure(x,y) ->
         match (evalMeasure head optionsR changes) with
         | Some(m) ->
            // Concatenate and recurse on tail
            let newList = singleMeasureList @ [m]
            evalAllMeasures tail optionsR newList {| Time = false; Capo = false; Key = false |}
         | None -> None

      // if it's an option, edit the optionsR
      | ScoreOption(key,value) ->

         match (parseOptions head optionsR) with
         | Some(newOptionsR) ->

            // figure out what changed
            let keyChange = not (newOptionsR.Key = optionsR.Key)
            let timeChange = not (newOptionsR.Time = optionsR.Time)
            let capoChange = not (newOptionsR.Capo = optionsR.Capo)

            evalAllMeasures tail newOptionsR singleMeasureList {| Time = timeChange; Key = keyChange; Capo = capoChange |}

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
         // subtract 75 to be the start of the next line
         let newY = y - 75.0
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
