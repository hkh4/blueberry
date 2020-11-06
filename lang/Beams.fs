module Beams

open System
open Types

// ################## DRAW THE BEAMS ###################

(* used to draw stubs at the end of a note
1) lastLocation is the x and y coords
2) lastRhythm is the Rhythm of the last note
3) isGrace is true if these notes are grace notes, false else
note: does similar thing as initialStubs but the required info is figured out within the method, for simplicity
RETURNS list of strings to be printed
*)
let endingStubs (lastLocation: float * float) (lastRhythm: Rhythm) (isGrace: bool) : string List =
   let (oldX,oldY) = lastLocation
   let (previousRhythmNumber,previousDots) =
      match lastRhythm with
      | R(x,n) -> x,n
      | _ ->
         printfn "Error in endingStubs: lastRhythm was of type Other, should be R(x,n)"
         X0,0 // SHOULD NEVER REACH THIS CASE
   let beamsOfPrevious = numberOfBeams.[previousRhythmNumber]

   // helper function to do the actual drawing
   let rec endingStubsHelper (x: float) (y: float) (toAdd: int List) (text: string List) (isGrace: bool) : string List =
      match toAdd with
      | [] -> text
      | head::tail ->
         let stub =
            match isGrace with
            // not a grace note
            | false ->
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
            // grace note
            | true ->
               match head with
               | 1 ->
                  [" 1.1 setlinewidth " + string (x - 0.9) + " " + string (y + 40.0) + " moveto " + string (x + 1.4) + " " + string (y + 40.0) + " lineto stroke "]
               | 2 ->
                  [" 1.1 setlinewidth " + string (x - 0.9) + " " + string (y + 38.5) + " moveto " + string (x + 1.4) + " " + string (y + 38.5) + " lineto stroke "]
               | 3 ->
                  [" 1.1 setlinewidth " + string (x - 0.9) + " " + string (y + 37.0) + " moveto " + string (x + 1.4) + " " + string (y + 37.0) + " lineto stroke "]
               | 4 ->
                  [" 1.1 setlinewidth " + string (x - 0.9) + " " + string (y + 35.5) + " moveto " + string (x + 1.4) + " " + string (y + 35.5) + " lineto stroke "]
               | _ -> [""] //should never reach this
         endingStubsHelper x y tail (text @ stub) isGrace

   endingStubsHelper oldX oldY [1..beamsOfPrevious] [] isGrace





(* used to draw stubs at the beginning of a note
1) x is the x coord
2) y is the y coord
3) toAdd is the list of ints which is the beams to draw
4) text is a string list that is composed as the function recurses
5) isGrace is true if these notes are grace notes, false else
RETURNS the list of strings
*)
let rec initialStubs (x: float) (y: float) (toAdd: int List) (text: string List) (isGrace: bool) : string List =
   match toAdd with
   | [] -> text
   | head::tail ->
      let stub =
         match isGrace with
         // not a grace note
         | false ->
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
         // grace note
         | true ->
            match head with
            | 1 ->
               [" 1.1 setlinewidth " + string (x + 1.1) + " " + string (y + 40.0) + " moveto " + string (x + 3.6) + " " + string (y + 40.0) + " lineto stroke "]
            | 2 ->
               [" 1.1 setlinewidth " + string (x + 1.1) + " " + string (y + 38.5) + " moveto " + string (x + 3.6) + " " + string (y + 38.5) + " lineto stroke "]
            | 3 ->
               [" 1.1 setlinewidth " + string (x + 1.1) + " " + string (y + 37.0) + " moveto " + string (x + 3.6) + " " + string (y + 37.0) + " lineto stroke "]
            | 4 ->
               [" 1.1 setlinewidth " + string (x + 1.1) + " " + string (y + 35.5) + " moveto " + string (x + 3.6) + " " + string (y + 35.5) + " lineto stroke "]
            | _ ->
               printfn "Error in initialStubs: more than 4 beams to be drawn"
               [""] //should never reach this
      initialStubs x y tail (text @ stub) isGrace





(* draws the full beams between notes
1) x is the x coord of the last note
2) newX is the x coord of the current note
3) y is the y coord
4) beamsToAdd is the list of ints which are the beams to be drawn
5) text is the string list
6) isGrace is true if these notes are grace notes, false else
RETURNS a string list to be printed
*)
let rec fullBeams (x: float) (newX: float) (y: float) (beamsToAdd: int List) (text: string List) (isGrace: bool) : string List =
   match beamsToAdd with
   | [] -> text
   | head::tail ->
      let stub =
         match isGrace with
         // not a grace note
         | false ->
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
         // grace note
         | true ->
            match head with
            | 1 ->
               [" 1.1 setlinewidth " + string (x + 1.0) + " " + string (y + 40.0) + " moveto " + string (newX + 1.5) + " " + string (y + 40.0) + " lineto stroke "]
            | 2 ->
               [" 1.1 setlinewidth " + string (x + 1.0) + " " + string (y + 38.5) + " moveto " + string (newX + 1.5) + " " + string (y + 38.5) + " lineto stroke "]
            | 3 ->
               [" 1.1 setlinewidth " + string (x + 1.0) + " " + string (y + 37.0) + " moveto " + string (newX + 1.5) + " " + string (y + 37.0) + " lineto stroke "]
            | 4 ->
               [" 1.1 setlinewidth " + string (x + 1.0) + " " + string (y + 35.5) + " moveto " + string (newX + 1.5) + " " + string (y + 35.5) + " lineto stroke "]
            | _ ->
               printfn "Error in fullBeams: a note can only have 1 2 3 or 4 beams"
               [""] //should never reach this

      fullBeams x newX y tail (text @ stub) isGrace





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
8) isGrace is true if these notes are grace notes, false else
RETURNS a list of strings and an int which is the next lastBeamed
*)
let beamByTime (key: int list list) (lastLocation: float * float) (lastRhythm: Rhythm) (lastStart: float) (head: Element) (lastBeamed: int) (lastLastRhythm: Rhythm) (isGrace: bool) : string List * int =
   let (x,y) = lastLocation
   // decompose lastRhythm into its rhythmNumber and dots
   let (previousRhythmNumber,previousDots) =
      match lastRhythm with
      | R(x,n) -> x,n
      | _ ->
         printfn "Error in beamByTime: lastRhythm was of type Other, should be R(x,n)"
         X0,0 // SHOULD NEVER REACH THIS CASE
   // decompose current rhythm into its rhythmNumber and dots
   let (currentRhythmNumber,currentDots) =
      match head.Duration with
      | R(x,n) -> x,n
      | _ ->
         printfn "Error in beamByTime: head.Duration was of type Other, should be R(x,n)"
         X0,0 // SHOULD NEVER REACH THIS CASE

   (* helper to reuse code
   1) lastLocation is the x y coords of the last note
   2) lastRhythm is the rhythm of the last note
   3) lastStart is the start beat of the last note
   4) head is the current element
   5) lastBeamed is 1 if the two previous notes were beamed and the same number of lines, 2 if the two previous notes were beamed but the first had more beams, 3 if the two previous notes were beamed but the second had more beams, and 0 else
   6) lastLastRhythm is two rhythms ago
   7) isGrace is true if these notes are grace notes, false else
   RETURNS a list of strings and an int which is the next lastBeamed
   *)
   let beamByTimeHelper (lastLocation: float * float) (lastRhythm: Rhythm) (lastStart: float) (head: Element) (lastBeamed: int) (lastLastRhythm: Rhythm) (isGrace: bool) : string List * int =

      // number of beams for the previous note
      let beamsOfPrevious = numberOfBeams.[previousRhythmNumber]

      // number of beams for the current note
      let beamsOfCurrent = numberOfBeams.[currentRhythmNumber]
      let (newX,newY) = head.Location

      match beamsOfPrevious with
      // if this note and the last are the same rhythm
      | num when num = beamsOfCurrent ->
         // just draw the full beams, lastBeamed = 1
         let equalBeams = fullBeams x newX y [1..num] [] isGrace
         (equalBeams,1)
      // if the last note has more beams than the current
      | num when num > beamsOfCurrent ->
         let equalBeams = fullBeams x newX y [1..beamsOfCurrent] [] isGrace

         match lastBeamed with

         | 0 ->
            // if the last and lastlast were not beamed, then the last note needs an initial stub since it needs more beams than the current
            let iStubs = initialStubs x y [1..num] [] isGrace
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
                  printfn "Error in beamByTime! If the lastBeamed is 3, then lastLastRhythm cannot by of type Other"
                  X0
            // beams for last last
            let numberOfBeamsLastLast = numberOfBeams.[lastLastRhythmNumber]
            match numberOfBeamsLastLast with
            // if this note has more beams than last last, then last gets an initial stub
            | num when num < beamsOfCurrent ->
               let iStubs = initialStubs x y [1..beamsOfPrevious] [] isGrace
               ((equalBeams @ iStubs),2)
            // if this note has less beams than last last, then last note gets an end stub
            | _ ->
               let endStubs = endingStubs lastLocation lastRhythm isGrace
               ((equalBeams @ endStubs),2)

         | _ ->
            printfn "Error in beamByTime: lastBeamed can only be 0 1 2 or 3"
            ([""],0) //should never reach this case
      // if this note has more beams than the last, draw just the beams, and a future case will take care of stubs

      | _ ->
         let equalBeams = fullBeams x newX y [1..beamsOfPrevious] [] isGrace
         (equalBeams,3)


   let indexOfLast = findElementInKey key (int lastStart) 0
   let indexOfCurrent = findElementInKey key (int head.Start) 0

   match (indexOfLast) with
   // if they're in the same group, beam
   | num when num = indexOfCurrent ->
      beamByTimeHelper lastLocation lastRhythm lastStart head lastBeamed lastLastRhythm isGrace
   | _ ->
      // if they're not in the same group, beam if the previous element went PAST the exact start of the current beat. e.g. last start was 2.9, current start is 3.1, not exactly 3.0, so beam
      // float int would turn 1.xxx into 1.0
      let wholeNumber = float (int head.Start)
      let difference = head.Start - wholeNumber
      match difference with
      | num when num > 0.0 ->
         beamByTimeHelper lastLocation lastRhythm lastStart head lastBeamed lastLastRhythm isGrace
      | _ ->
         // may need to add a flag to the last note
         match lastBeamed with
         | 3 ->
            let flag = endingStubs lastLocation lastRhythm isGrace
            (flag,0)
         | 0 ->
            let flag = drawFlags x y lastRhythm isGrace
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
9) isGrace is true if these notes are grace notes, false else
RETURNS: a list of strings to be printed, and an int which is the new lastBeamed
*)
let beamHelper (head: Element) (s: int) (lastLocation: float * float) (lastRhythm: Rhythm) (lastStart: float) (timeSignature: int * int) (lastBeamed: int) (lastLastRhythm: Rhythm) (isGrace: bool) : string List * int =
   let dotNumber =
      match head.Duration with
      | R(x,n) -> n
      | Other -> 0
   let (x,y) = head.Location
   let (oldX,oldY) = lastLocation
   // draw the stem, depending on which string it's on. If it's on string 6, the beam has to be a little shorter
   let stem =
      match isGrace with
      // not a grace note
      | false ->
         match s with
         | 6 -> "0.7 setlinewidth " + string (x + 2.0) + " " + string (y + 33.0) + " moveto 0 9 rlineto stroke "
         | _ -> "0.7 setlinewidth " + string (x + 2.0) + " " + string (y + 32.0) + " moveto 0 10 rlineto stroke "
      // grace note
      | true ->
         match s with
         | 6 -> "0.5 setlinewidth " + string (x + 1.25) + " " + string (y + 32.3) + " moveto 0 7.7 rlineto stroke "
         | _ -> "0.5 setlinewidth " + string (x + 1.25) + " " + string (y + 32.0) + " moveto 0 8 rlineto stroke "

   // draw the dots, depending on how many
   let dots =
      match isGrace with
      // not a grace note
      | false ->
         match dotNumber with
         | 0 -> ""
         | 1 -> dotTemplate (x + 4.2) (y + 33.4) isGrace
         | 2 -> dotTemplate (x + 4.2) (y + 33.4) isGrace + dotTemplate (x + 5.9) (y + 33.4) isGrace
         | 3 -> dotTemplate (x + 4.2) (y + 33.4) isGrace + dotTemplate (x + 5.9) (y + 33.4) isGrace + dotTemplate (x + 7.6) (y + 33.4) isGrace
         | _ ->
            printfn "Error in beamHelper! Note had more than 3 dots"
            ""
      // grace note
      | true ->
         match dotNumber with
         | 0 -> ""
         | 1 -> dotTemplate (x + 3.0) (y + 33.0) isGrace
         | 2 -> dotTemplate (x + 3.0) (y + 33.0) isGrace + dotTemplate (x + 4.4) (y + 33.0) isGrace
         | 3 -> dotTemplate (x + 3.0) (y + 33.0) isGrace + dotTemplate (x + 4.4) (y + 33.0) isGrace + dotTemplate (x + 5.8) (y + 33.0) isGrace
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

            // For 5s, group 3 and 2 by default
            | (n,m) when (m = 8 || m = 16 || m = 32 || m = 64) && (n = 5) ->
               [[1;2;3];[4;5]]

            // for 7s, group 3 2 2
            | (n,m) when (m = 8 || m = 16 || m = 32 || m = 64) && (n = 7) ->
               [[1;2;3];[4;5];[6;7]]

            | _ ->
               printfn "Error: this time signature has not yet been fully implemented. Sorry!"
               [[0]]
         // call beamByTime, return the strings of the beams and the int for the new lastBeamed
         let (newText, newLastBeamed) = beamByTime key lastLocation lastRhythm lastStart head lastBeamed lastLastRhythm isGrace
         // return
         (([stem] @ newText @ [dots]),newLastBeamed)
      // If the current note is quarter half or whole
      | _ ->
         // check to see if there's an end stub to be drawn
         match lastBeamed with
         | 3 ->
            let endStubs = endingStubs lastLocation lastRhythm isGrace
            (([stem] @ endStubs @ [dots]),0)
         | 0 ->
            // check to see if flags are needed on the previous note
            let flag = drawFlags oldX oldY lastRhythm isGrace
            (([stem] @ [dots] @ flag),0)
         | _ ->
            (([stem]@[dots]),0)




(* Draw the brackets on top of tuplets
1) start is the location of the first note or first grace note
2) ending is the location of the last note
3) nList is all the Elements of the tuplet
RETURNS a list of strings
*)
let drawTupletBracket (start: float * float) (ending: float * float) (tupletNotes: Element List) : string List =

   // Figure out the number for the bracket
   // First, add up the rhythms. Here's a helper method to do that
   let rec addRhythms (notes: Element List) (acc: float) : float =
      match notes with
      // if there are no more notes, return the accumulator
      | [] -> acc
      | head::tail ->
         let thisDuration =
            match head.Duration with
            | R(rNumber, dots) ->

               // Based on the rhythm, get a number that represents it
               let baseRhythm =
                  match rNumber with
                  | X0 ->
                     printfn "Error in drawTupletBracket! A note should have a RhythmNumber of X0"
                     -1.0
                  | X1 -> 4.0
                  | X2 -> 2.0
                  | X4 -> 1.0
                  | X8 -> 0.5
                  | X16 -> 0.25
                  | X32 -> 0.125
                  | X64 -> 0.0625
               let baseTimesDots =
                  match dots with
                  | 0 -> baseRhythm
                  | 1 -> baseRhythm * 1.5
                  | 2 -> baseRhythm * 1.75
                  | _ -> baseRhythm * 1.875

               baseTimesDots

            | Other ->
               match (head.NoteInfo) with
               | Buffer -> 0.0
               | _ ->
                  printfn "Error in drawTupletBracket! A note should not have a Rhythm of Other"
                  0.0

         addRhythms tail (acc + thisDuration)

   let totalRhythm = addRhythms tupletNotes 0.0

   // Second , multiply by 16 to figure out how many 64th notes there are, and then calculate which is the base rhythm and how many there are

   let rhythmIn64 = totalRhythm * 16.0

   let rec calculateBaseRhythm (runningTotal: float) (acc: int) : int * int =
      match acc with
      // if the accumulator is 6, then it must be a whole note
      | num when num >= 7 ->
         let i = int runningTotal
         (i,7)
      | num ->

         // Check to see if this can be divided in half and be whole
         match (runningTotal / 2.0) with

         // If it can be divided by 2, recurse
         | n when n = Math.Floor(runningTotal / 2.0) ->
            calculateBaseRhythm n (acc + 1)

         // If it cannot be divided by 2, this is the end
         | n ->
            let i = int runningTotal
            (i,acc)

   let (multiplier, tupletBase) = calculateBaseRhythm rhythmIn64 1

   // Not using tupletBase for now but it could be used in the future

   // Create the string
   let (startX, startY) = start
   let (endX, endY) = ending

   [" " + string startX + " " + string startY + " " + string endX + " " + string endY + " " + string multiplier + " tupletBracket "]





(* Driver for drawing beams
1) els is the list of Elements in this measure
2) text is the list of strings
3) lastLocation is the x-y coords of the last element
4) lastRhythm is the Rhythm of the last note
5) lastStart is the start beat of the last note
6) timeSignature is the time signature
7) lastBeamed is 1 if the two previous notes were beamed and the same number of lines, 2 if the two previous notes were beamed but the first had more beams, 3 if the two previous notes were beamed but the second had more beams, and 0 else
8) lastLastRhythm is two rhythms ago
9) isGrace is true if these notes are grace notes, false else
RETURNS a list of strings
*)
let rec beam (els: Element List) (text: string List) (lastLocation: float * float) (lastRhythm: Rhythm) (lastStart: float) (timeSignature: int * int) (lastBeamed: int) (lastLastRhythm: Rhythm) (isGrace: bool) : string List =

   match els with
   // Base case: no more elements
   | [] ->
      // since the last element is always a barline, or a buffer in the case of the grace notes, nothing should have to be done at the end
      text
   | head::tail ->

      match head.NoteInfo with

      | SingleNote(n,mProperties) ->
         match n with
         // guitar note - beam
         | NormalGuitarNote(guitarString,pitch,fret,eProperties) ->
            let (newText, newLastBeamed) = beamHelper head guitarString lastLocation lastRhythm lastStart timeSignature lastBeamed lastLastRhythm isGrace
            beam tail (text @ newText) head.Location head.Duration head.Start timeSignature newLastBeamed lastRhythm isGrace
         // also beam if an X
         | X(guitarString,eProperties) ->
            let (newText, newLastBeamed) = beamHelper head guitarString lastLocation lastRhythm lastStart timeSignature lastBeamed lastLastRhythm isGrace
            beam tail (text @ newText) head.Location head.Duration head.Start timeSignature newLastBeamed lastRhythm isGrace

      | GroupNote(nList,mProperties) ->

         // helper method to find out if any of the notes in the group are on string 6 for stem purposes
         let rec findString6 (nList: singleNote List) : bool =
            match nList with
            | [] -> false // random number that isn't 6
            | head::tail ->
               match head with
               | NormalGuitarNote(guitarString,pitch,fret,eProperties) ->
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
            | true -> beamHelper head 6 lastLocation lastRhythm lastStart timeSignature lastBeamed lastLastRhythm isGrace
            // otherwise, not 6. 1 chosen at random
            | _ -> beamHelper head 1 lastLocation lastRhythm lastStart timeSignature lastBeamed lastLastRhythm isGrace
         // recurse
         beam tail (text @ newText) head.Location head.Duration head.Start timeSignature newLastBeamed lastRhythm isGrace

      | TupletNote(nList) ->

         // simply add the notes within to the larger list of notes

         let allNotes = nList @ tail

         // find the first and last note for their information
         let lastItem = nList.Item(nList.Length - 1)
         let firstItem = nList.Head

         // draw the tuplet bracket
         let tupletBracket = drawTupletBracket firstItem.Location lastItem.Location nList

         beam allNotes (text @ tupletBracket) lastLocation lastRhythm lastStart timeSignature lastBeamed lastLastRhythm false

      | _ ->
         // add the end slur for grace notes

         let endCurve =
            let (oldX,oldY) = lastLocation
            let (newX,newY) = head.Location
            match isGrace with
            | true -> [ string (oldX + 2.0) + " " + string (oldY + 42.0) + " " + string (newX + 1.0) + " " + string (newY + 44.0) + " graceCurve " ]
            | false -> [""]


         match lastBeamed with
         | 3 ->
            // check if end stubs are needed if this note is not a note but the last note might need a stub
            let endStubs = endingStubs lastLocation lastRhythm isGrace
            beam tail (text @ endStubs @ endCurve) (0.0,0.0) Other 0.0 timeSignature 0 lastRhythm isGrace
         | 0 ->
            // last note might need flags
            let (oldX,oldY) = lastLocation
            let flag = drawFlags oldX oldY lastRhythm isGrace
            beam tail (text @ flag @ endCurve) (0.0,0.0) Other 0.0 timeSignature 0 lastRhythm isGrace
         | _ ->
            beam tail (text @ endCurve) (0.0,0.0) Other 0.0 timeSignature 0 lastRhythm isGrace




(* used to beam grace notes, calls the beam method used for all beaming
1) els is the list of elements of the measure
2) text is the list of strings to print
3) time is the time signature
RETURNS list of strings
*)
let rec beamGraceNotes (els: Element List) (text: string List) (time: int * int) : string List =
   match els with
   | [] -> text
   | head::tail ->

      match head.NoteInfo with
      // if it's a tuplet, look for grace notes within
      | TupletNote(nList) ->

         let tupleGraceText = beamGraceNotes nList [] time
         beamGraceNotes tail (text @ tupleGraceText) time

      | _ ->
         // draw the slash at the beginning of the grace note
         let slash =

            match head.GraceNotes with
            // if there are no grace notes, do nothing
            | [] -> [""]
            // otherwise, draw it
            | h::t ->
               let (x,y) = h.Location
               [" 0.4 setlinewidth " + string (x - 0.5) + " " + string (y + 33.0) + " moveto 4 4.4 rlineto stroke"]
         // beam the grace notes, if there any

         let graceNoteBeams = beam head.GraceNotes [] (0.0,0.0) Other 0.0 time 0 Other true
         beamGraceNotes tail (text @ graceNoteBeams @ slash) time
