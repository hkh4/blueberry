module Properties

open System
open Types

(* Draw a slur
1) isGrace is a bool that says whether or not this note is a grace note
2) currentX is the x coord for this element
3) currentY is the y coord for this element
4) mProperties is the multi property list for this element
5) propertyList describes the properties to be drawn
6) measureNumber is the measure number
RETURNS the list of strings and the new slurStart
*)
let drawSlur (isGrace: bool) (currentX: float) (currentY: float) (mProperties: MultiProperty List) (propertyList: PropertyList) (measureNumber: int) : (String List * PropertyList) option =

   // does this element have Sls
   let hasSls = List.exists (fun e -> e = Sls) mProperties
   // does this element have Sle
   let hasSle = List.exists (fun e -> e = Sle) mProperties

   match (hasSls,hasSle) with
   // if a note has both sls and sle, error
   | (true,true) ->
      printfn "Error in measure %i! A note can't have both slur start and slur end!" measureNumber
      None
   // if a note has sls but not sle
   | (true,false) ->
      match propertyList.SlurStart with
      // there was already a slur started
      | ((x,y),b,g) when b = true ->
         printfn "Error in measure %i! Overlapping slurs detected" measureNumber
         None
      // return an empty list, and the new slurStart for recursion
      | ((x,y),b,g) ->
         let newPropertyList = { propertyList with SlurStart = ((currentX,currentY),true,isGrace) }
         Some([""],newPropertyList)
   // has the end slur
   | (false,true) ->
      match propertyList.SlurStart with
      // there was a slur started
      | ((x,y),b,g) when b = true ->
         let diff = currentX - x
         let slurCommand =
            match isGrace with
            | true when g = true -> " slurgrace"
            | _ ->
               match diff with
               | num when num > 150.0 -> " slurverylong "
               | num when num > 80.0 -> " slurlong "
               | num when num < 20.0 -> " slurshort "
               | _ -> " slur "

         let slurString = string x + " " + string y + " " + string currentX + " " + string currentY + slurCommand
         let newPropertyList = { propertyList with SlurStart = ((0.0,0.0),false,false) }
         Some([slurString],newPropertyList)
      // no slur started, error
      | ((x,y),b,g) ->
         printfn "Error in measure %i! A slur was marked as ended but there was no beginning slur" measureNumber
         None
   // doesn't have start or end slur
   | (false,false) -> Some([""],propertyList)





(* Draw longer palm mutes
1) currentX is the x coord
2) currentY is the y coord
3) mList is the list of multiproperties
4) pList is the PropertyList
5) measureNumber is the number of the current measure
6) isGrace is whether or not this is a grace note
RETURNS list of strings to be printed, and the updated PropertyList
*)
let drawLongPalmMute (currentX: float) (currentY: float) (mList: MultiProperty List) (propertyList: PropertyList) (measureNumber: int) (isGrace: bool) : (String List * PropertyList) option =

   // does this element have Pl1
   let hasPl1 = List.exists (fun e -> e = Pl1) mList
   // does this element have Pl2
   let hasPl2 = List.exists (fun e -> e = Pl2) mList

   match (hasPl1,hasPl2) with
   // if a note has both pl1 and pl2, error
   | (true,true) ->
      printfn "Error in measure %i! A note can't both start and end a palm mute!" measureNumber
      None
   // if a note has pl1 but not pl2
   | (true,false) ->
      match propertyList.MuteStart with

      // there was already a mute started
      | ((x,y),v) when v = true ->
         printfn "Error in measure %i! Overlapping palm mutes detected!" measureNumber
         None

      // return an empty list, and the new muteStart
      | ((x,y),v) ->
         let newPropertyList = { propertyList with MuteStart = ((currentX,currentY),true) }
         Some([""],newPropertyList)

   // has the end mute
   | (false,true) ->
      match propertyList.MuteStart with
      // there was a mute started
      | ((x,y),v) when v = true ->

         let muteCommand = " longpalmmute "
         let newCurrentX =
            match isGrace with
            | true -> currentX - 0.5
            | false -> currentX

         let muteString = string x + " " + string y + " " + string newCurrentX + " " + string currentY + muteCommand
         let newPropertyList = { propertyList with MuteStart = ((0.0,0.0),false) }
         Some([muteString],newPropertyList)
      // no mute started, error
      | ((x,y),v) ->
         printfn "Error in measure %i! A palm mute was marked as ended but there was no beginning mute" measureNumber
         None
   // doesn't have start or end mute
   | (false,false) -> Some([""],propertyList)





(* Helper method for drawing tie properties
1) currentString is the guitar string of the note
2) eProperties is the list of EitherProperty
3) pitch is the pitch of the note
4) fret is the fret of the current note (-1 if an X)
5) yCoord is the y coordinate based on which string the note is on
6) propertyList is the record that describes the state of properties to be drawn
7) isGrace is a bool that says whether or not this note is a grace note
8) xCoord is the xcoord
RETURNS the list of strings to be printed and the new PropertyList
*)
let drawTie (currentString: int) (eProperties: EitherProperty List) (pitch: Pitch) (fret: int) (yCoord: float) (propertyList: PropertyList) (isGrace: bool) (xCoord: float) : (string List * PropertyList) option =

   // get the tiestart for this string
   let elem = propertyList.TieStart.[currentString]

   // flag
   let mutable drawn = false;

   // get the text and the new property list if a tie is needed
   let text =
      match elem with
      // If there was a tie requested from a previous note on this string
      | ((x,y),f,g,b) when b = true ->

         match f with
         // if the frets match, draw the tie
         | fr when fr = fret ->
            drawn <- true
            // figure out which tie function to use
            let tieFunction =
               match isGrace with
               | true ->
                  match g with
                  // both grace
                  | true -> " tiegrace "
                  // second grace
                  | false -> " tiegracesecond "
               | false ->
                  match g with
                  // first grace
                  | true -> " tiegracefirst "
                  // neither grace
                  | false -> " tie "

            let tempText = string x + " " + string y + " " + string xCoord + " " + string yCoord + " " + string fret + tieFunction

            [tempText]

         | fr' -> []

      | ((x,y),f,g,b) -> []

   // see if this note wanted a tie
   let newPropertyList =

      match (List.exists (fun e -> e = Tie) eProperties) with
      // wants a tie
      | true ->
         // update the propertyList
         let newTieList = propertyList.TieStart.Remove(currentString).Add(currentString,((xCoord,yCoord),fret,isGrace,true))
         { propertyList with TieStart = newTieList }

      // no tie, just return
      | false ->

         match drawn with
         //if a tie was drawn, zero it out
         | true ->

            // set the propertyList element for this string to empty
            let newTieList = propertyList.TieStart.Remove(currentString).Add(currentString,((0.0,0.0),0,false,false))
            { propertyList with TieStart = newTieList }

         | false -> propertyList

   Some(text,newPropertyList)






(* Helper method for drawing hammer properties
1) currentString is the guitar string of the note
2) eProperties is the list of EitherProperty
3) fret is the fret of the current note (-1 if an X)
4) xCoord is the x coord
5) yCoord is the y coord based on what string the note is on
6) propertyList is the record that describes the state of properties to be drawn
7) isGrace is a bool that says whether or not this note is a grace note
RETURNS the list of strings to be printed and the new PropertyList
*)
let drawHammer (currentString: int) (eProperties: EitherProperty List) (fret: int) (xCoord: float) (yCoord: float) (propertyList: PropertyList) (isGrace: bool) : (string List * PropertyList) option =

   // get the hammer start for this string
   let elem = propertyList.HammerStart.[currentString]

   // get the text and the new property list if a hammer is needed
   let text =
      match elem with
      // If there was a slide requested from a previous note on this string
      | ((x,y),f,g,b) when b = true ->

         // grace note?
         let whichFunction =

            // is the first note a grace note?
            match g with
            | true ->

               // is the second note a grace note?
               match isGrace with
               | true -> "hammerbothgrace "
               | false -> "hammerfirstgrace "

            | false ->
               match isGrace with
               | true -> "hammersecondgrace "
               | false -> "hammer "

         [" " + string x + " " + string y + " " + string xCoord + " " + string yCoord + " " + string f + " " + string fret + " " + whichFunction]

      | ((x,y),f,g,b) -> []

   // see if this note wanted a hammer
   let newPropertyList =

      match (List.exists (fun e -> e = Ham) eProperties) with
      // wants a hammer
      | true ->
         // update the propertyList
         let newHammerList = propertyList.HammerStart.Remove(currentString).Add(currentString,((xCoord,yCoord),fret,isGrace,true))
         { propertyList with HammerStart = newHammerList }

      // no hammer, just return
      | false ->
         // set the propertyList element for this string to empty
         let newHammerList = propertyList.HammerStart.Remove(currentString).Add(currentString,((0.0,0.0),0,false,false))
         { propertyList with HammerStart = newHammerList }

   Some(text,newPropertyList)






(* Helper method for drawing slide properties
1) currentString is the guitar string of the note
2) eProperties is the list of EitherProperty
3) pitch is the pitch of the note
4) fret is the fret of the current note (-1 if an X)
5) yCoord is the y coordinate based on which string the note is on
6) propertyList is the record that describes the state of properties to be drawn
7) isGrace is a bool that says whether or not this note is a grace note
8) xCoord is the xcoord
RETURNS the list of strings to be printed and the new PropertyList
*)
let drawSlide (currentString: int) (eProperties: EitherProperty List) (pitch: Pitch) (fret: int) (yCoord: float) (propertyList: PropertyList) (isGrace: bool) (xCoord: float) : (string List * PropertyList) option =

   // get the slidestart for this string
   let elem = propertyList.SlideStart.[currentString]

   // get the slidestub for this string
   let stub = propertyList.SlideStubs.[currentString]

   // get the text and the new property list if a slide is needed
   let text =
      match elem with
      // If there was a slide requested from a previous note on this string
      | ((x,y),f,g,b) when b = true ->

         // figure out which slide function to use
         let direction =
            match f with
            // if the last fret is smaller or the same, slide up
            | num when num <= fret ->
               match g with
               // both are grace notes
               | true when isGrace = true -> " slideupbothgrace"
               // first one is grace
               | true -> " slideupfirstgrace "
               // second one is grace
               | false when isGrace = true -> " slideupsecondgrace "
               // neither are grace notes
               | false -> " slideup "

            // slide down
            | _ ->
               match g with
               | true when isGrace = true -> " slidedownbothgrace "
               | true -> " slidedownfirstgrace "
               | false when isGrace = true -> " slidedownsecondgrace "
               | false -> " slidedown "


         let tempText = string f + " " + string fret + " " + string x + " " + string y + " " + string xCoord + " " + string yCoord + direction

         // if true, there's a stub to be drawn
         match stub with
         | ((x',y'),f',g',b') when b' = true ->

            let stubText = string f' + " " + string fret + " " + string x' + " " + string y' + " " + string (x' + 15.0) + " " + string y' + direction
            [tempText] @ [stubText]

         | ((x',y'),f',g',b') ->

            [tempText]

      | ((x,y),f,g,b) -> []

   // see if this note wanted a slide
   let newPropertyList =

      match (List.exists (fun e -> e = Sli) eProperties) with
      // wants a slide
      | true ->
         // update the propertyList
         let newSlideList = propertyList.SlideStart.Remove(currentString).Add(currentString,((xCoord,yCoord),fret,isGrace,true))
         { propertyList with SlideStart = newSlideList }

      // no slide, just return
      | false ->
         // set the propertyList element for this string to empty
         let newSlideList = propertyList.SlideStart.Remove(currentString).Add(currentString,((0.0,0.0),0,false,false))
         { propertyList with SlideStart = newSlideList }

   // set the slidestub to empty
   let newSlideStubs = newPropertyList.SlideStubs.Remove(currentString).Add(currentString,((0.0,0.0),0,false,false))
   let newerPropertyList = { newPropertyList with SlideStubs = newSlideStubs }

   Some(text,newerPropertyList)





(* Helper to draw slide-ups
1) eProperties is the list of EitherProperty
2) y is the y-coord
3) x is the x-coord
4) isGrace is a bool that says whether or not this note is a grace note
5) fret is the fret of this note
RETURNS the list of strings
*)
let drawSlideUp (eProperties: EitherProperty List) (y: float) (x: float) (isGrace: bool) (fret: int) : (string List) option =
   match (List.exists (fun e -> e = Slu) eProperties) with
   | true ->
      let text =
         match isGrace with
         | true ->
            " " + string x + " " + string y + " " + string fret + " supgrace "
         | false ->
            " " + string x + " " + string y + " " + string fret + " sup "
      Some([text])
   | false -> Some([])





(* Helper to draw slide-downs
1) eProperties is the list of EitherProperty
2) y is the y-coord
3) x is the x-coord
4) isGrace is a bool that says whether or not this note is a grace note
5) fret is the fret of this note
RETURNS the list of strings
*)
let drawSlideDown (eProperties: EitherProperty List) (y: float) (x: float) (isGrace: bool) (fret: int) : (string List) option =
   match (List.exists (fun e -> e = Sld) eProperties) with
   | true ->
      let text =
         match isGrace with
         | true -> " " + string x + " " + string y + " " + string fret + " sdngrace "
         | false -> " " + string x + " " + string y + " " + string fret + " sdn "
      Some([text])
   | false -> Some([])





(* Helper to draw slide-downs
1) eProperties is the list of EitherProperty
2) y is the y-coord
3) x is the x-coord
4) isGrace is a bool that says whether or not this note is a grace note
5) fret is the fret of this note
RETURNS the list of strings
*)
let drawParens (eProperties: EitherProperty List) (y: float) (x: float) (isGrace: bool) (fret: int) : (string List) option =

   match (List.exists (fun e -> e = Par) eProperties) with
   | true ->

      match isGrace with
      | true -> Some([" " + string (x - 1.4) + " " + string (y + 1.0) + " " + string fret + " openPG "; " " + string (x + 2.6) + " " + string (y + 1.0) + " " + string fret + " closePG "])

      | false -> Some([" " + string (x - 1.7) + " " + string (y + 0.5) + " " + string fret + " openP "; " " + string (x + 3.5) + " " + string (y + 0.5) + " " + string fret + " closeP "])

   | false -> Some([""])





(* Draw strum up
1) isGrace is a bool that says whether or not this note is a grace note
2) x is the x coord for this element
3) y is the y coord for this element
4) mProperties is the multi property list for this element
RETURNS the list of strings
*)
let drawStrumUp (isGrace: bool) (x: float) (y: float) (mList: MultiProperty List) (bottomString: int) (topString: int) : (string List) option =

   match (List.exists (fun e -> e = Stu) mList) with
   // If it includes Stu
   | true ->

      let height = topString - bottomString + 1

      match isGrace with
      | true ->
         Some([" " + string x + " " + string y + " " + string height + " " + string bottomString + " arrowGrace "])
      | false ->
         Some([" " + string x + " " + string y + " " + string height + " " + string bottomString + " arrow "])

   | false -> Some([""])





(* Draw strum down
1) isGrace is a bool that says whether or not this note is a grace note
2) x is the x coord for this element
3) y is the y coord for this element
4) mProperties is the multi property list for this element
RETURNS the list of strings
*)
let drawStrumDown (isGrace: bool) (x: float) (y: float) (mList: MultiProperty List) (bottomString: int) (topString: int) : (string List) option =

   match (List.exists (fun e -> e = Std) mList) with
   // If it includes Std
   | true ->

      let height = topString - bottomString + 1

      match isGrace with
      | true ->
         Some([" " + string x + " " + string y + " " + string height + " " + string topString + " arrowDownGrace "])
      | false ->
         Some([" " + string x + " " + string y + " " + string height + " " + string topString + " arrowDown "])

   | false -> Some([""])





(* Helper to draw harmonics
1) eProperties is the list of EitherProperty
2) y is the y-coord
3) x is the x-coord
4) isGrace is a bool that says whether or not this note is a grace note
5) fret is the fret of this note
RETURNS the list of strings
*)
let drawHarmonics (eProperties: EitherProperty List) (y: float) (x: float) (isGrace: bool) (fret: int) : (string List) option =

   match (List.exists (fun e -> e = Har) eProperties) with
   | true ->
      match isGrace with
      | true ->
         Some([" " + string x + " " + string y + " " + string fret + " harmonicsgrace "])

      | false ->
         Some([" " + string x + " " + string y + " " + string fret + " harmonics "])

   | false -> Some([""])




(* Draw pluck down
1) isGrace is a bool that says whether or not this note is a grace note
2) x is the x coord for this element
3) y is the y coord for this element
4) mProperties is the multi property list for this element
RETURNS the list of strings
*)
let drawPluckDown (isGrace: bool) (x: float) (y: float) (mList: MultiProperty List) (bottomString: int) (topString: int) : (string List) option =

   match (List.exists (fun e -> e = Pld) mList) with
   | true ->

      let height = topString - bottomString + 1

      match isGrace with
      | true -> Some([" " + string x + " " + string y + " " + string height + " " + string topString + " pluckdowngrace "])

      | false -> Some([" " + string x + " " + string y + " " + string height + " " + string topString + " pluckdown "])

   | false -> Some([""])





(* Draw pluck up
1) isGrace is a bool that says whether or not this note is a grace note
2) x is the x coord for this element
3) y is the y coord for this element
4) mProperties is the multi property list for this element
RETURNS the list of strings
*)
let drawPluckUp (isGrace: bool) (x: float) (y: float) (mList: MultiProperty List) (bottomString: int) (topString: int) : (string List) option =

   match (List.exists (fun e -> e = Plu) mList) with
   | true ->

      let height = topString - bottomString + 1

      match isGrace with
      | true -> Some([" " + string x + " " + string y + " " + string height + " " + string bottomString + " pluckupgrace "])

      | false -> Some([" " + string x + " " + string y + " " + string height + " " + string bottomString + " pluckup "])

   | false -> Some([""])





(* Draw palm mutes
1) x is the x coord
2) y is the y coord
3) mList is the list of multiproperties
RETURNS list of strings to be printed
*)
let drawPalmMute (x: float) (y: float) (mList: MultiProperty List) : (string List) option =

   match (List.exists (fun e -> e = Plm) mList) with
   | true -> Some([" " + string x + " " + string y + " palmmute "])

   | false -> Some([""])






(* Helper method for drawing the eProperties of a singleNote
1) currentString is the guitar string of the note
2) eProperties is the list of EitherProperty
3) pitch is the pitch of the note
4) fret is the fret of the current note (-1 if an X)
5) yCoord is the y coordinate based on which string the note is on
6) propertyList is the record that describes the state of properties to be drawn
7) isGrace is a bool that says whether or not this note is a grace note
8) x is the xcoord
9) y is the ycoord
10) measureNumber is the measure number
RETURNS the list of strings to be printed and the new PropertyList
*)
let drawEProperties (currentString: int) (eProperties: EitherProperty List) (pitch: Pitch) (fret: int) (yCoord: float) (propertyList: PropertyList) (isGrace: bool) (x: float) (y: float) (measureNumber: int) : (string List * PropertyList) option =

   // Draw slides
   match (drawSlide currentString eProperties pitch fret yCoord propertyList isGrace x) with
   | Some(slideText, propertyList') ->

      // draw ties
      match (drawTie currentString eProperties pitch fret yCoord propertyList' isGrace x) with
      | Some(tieText,propertyList'') ->

         match (drawSlideUp eProperties yCoord x isGrace fret) with
         | Some(slideUpText) ->

            match (drawSlideDown eProperties yCoord x isGrace fret) with
            | Some(slideDownText) ->

               match (drawParens eProperties yCoord x isGrace fret) with
               | Some(parensText) ->

                  match (drawHarmonics eProperties yCoord x isGrace fret) with
                  | Some(harText) ->

                     match (drawHammer currentString eProperties fret x yCoord propertyList'' isGrace) with
                     | Some(hamText,propertyList''') ->

                        Some((slideText @ tieText @ slideUpText @ slideDownText @ parensText @ harText @ hamText),propertyList''')

                     | None -> None
                  | None -> None
               | None -> None
            | None -> None
         | None -> None
      | None -> None
   | None -> None





(* Helper method for drawing the mProperties of an Element
1) mList is the list of multiProperties for this element
2) propertyList is the record that describes the state of properties to be drawn
3) isGrace is a bool that says whether or not this note is a grace note
4) x is the xcoord
5) y is the ycoord
6) bottomString is the lowest string in this element, used for groups
7) topString is the highest string in this element, used for groups
8) measureNumber is the measure number
RETURNS the list of strings to be printed and the new PropertyList
*)
let drawMProperties (mList: MultiProperty List) (propertyList: PropertyList) (isGrace: bool) (x: float) (y: float) (bottomString: int) (topString : int) (measureNumber: int) : (string List * PropertyList) option =
   // draw slurs
   match (drawSlur isGrace x y mList propertyList measureNumber) with
   // successful slur drawing
   | Some(slurList,propertyList') ->

      match (drawStrumUp isGrace x y mList bottomString topString) with
      | Some(strumUpList) ->

         match (drawStrumDown isGrace x y mList bottomString topString) with
         | Some(strumDownList) ->

            match (drawPluckUp isGrace x y mList bottomString topString) with
            | Some(pluckUpList) ->

               match (drawPluckDown isGrace x y mList bottomString topString) with
               | Some(pluckDownList) ->

                  match (drawPalmMute x y mList) with
                  | Some(palmMuteList) ->

                     match (drawLongPalmMute x y mList propertyList' measureNumber isGrace) with
                     | Some(longPalmList, propertyList'') ->

                        Some((slurList @ strumUpList @ strumDownList @ pluckUpList @ pluckDownList @ palmMuteList @ longPalmList),propertyList'')

                     | None -> None
                  | None -> None
               | None -> None
            | None -> None
         | None -> None
      | None -> None
   | None -> None





(* helper for drawing properties of a single element
1) el is the current Element
2) propertyList is the record that describes the state of properties to be drawn
3) isGrace is a bool that says whether or not this note is a grace note
4) measureNumber is the measure number
RETURNS the list of strings to be printed and the new PropertyList
*)
let rec drawPropertiesElement (el: Element) (propertyList: PropertyList) (isGrace: bool) (measureNumber: int) : (string List * PropertyList) option =

   match el.NoteInfo with
   | SingleNote(n,mProperties) ->
      // x y
      let (currentX,currentY) = el.Location

      // useful properties of the note
      let (currentString,pitchOfNote,fret,eProperties) =
         match n with
         | NormalGuitarNote(guitarString,pitch,fretTemp,eList) -> guitarString,pitch,fretTemp,eList
         | X(guitarString,eList) -> guitarString,NoPitch,-1,eList

      // used for eProperties
      let yCoord = (currentY - 2.3) + (6.0 * ((float currentString) - 1.0))

      //** draw the mProperties
      match (drawMProperties mProperties propertyList isGrace currentX currentY currentString currentString measureNumber) with
      | Some(mTextList,propertyList') ->

         // call the helper to draw eProperties
         match (drawEProperties currentString eProperties pitchOfNote fret yCoord propertyList' isGrace currentX currentY measureNumber) with
         | Some(eTextList,propertyList'') ->
            Some((mTextList @ eTextList), propertyList'')

         | None -> None
      | None -> None


   | GroupNote(nList,mProperties) ->
      // x y
      let (currentX, currentY) = el.Location



      (* Recursive helper to parse each note in the list and call the helper to draw the eProperties
      1) sList is the notes in this group
      2) t is the string list composed to be returned and printed
      3) pList is the propertyList describing how to draw some properties
      RETURNS none if an error, or the full text from all the eProperties and the new PropertyList
      *)
      let rec groupPropertiesHelper (sList: singleNote List) (t: string List) (pList: PropertyList) : (string List * PropertyList) option =
         match sList with
         | [] -> Some(t,pList)
         | head::tail ->

            // useful properties of the note
            let (currentString,pitchOfNote,fret,eProperties) =
               match head with
               | NormalGuitarNote(guitarString,pitch,fretTemp,eList) -> guitarString,pitch,fretTemp,eList
               | X(guitarString,eList) -> guitarString,NoPitch,-1,eList

            // specific y coord depending on which string the note is on
            let yCoord = (currentY - 2.3) + (6.0 * ((float currentString) - 1.0))

            match (drawEProperties currentString eProperties pitchOfNote fret yCoord pList isGrace currentX currentY measureNumber) with
            | Some(eT,pList') ->

               groupPropertiesHelper tail (t @ eT) pList'
            | None -> None



      let rec getStrings (sList: singleNote List) (t: int List) : int List =
         match sList with
         | [] -> t
         | head::tail ->
            let currentString =
               match head with
               | NormalGuitarNote(guitarString,pitch,fret,eList) -> guitarString
               | X(guitarString,eList) -> guitarString
            getStrings tail (currentString::t)


      // First, figure out which guitar strings are used, necessary for some of the properties
      let allStrings = List.sort (getStrings nList [])
      let firstString = allStrings.Head
      let secondString = allStrings.Item(allStrings.Length - 1)

      // draw the multiproperties
      match (drawMProperties mProperties propertyList isGrace currentX currentY firstString secondString measureNumber) with
      | Some(mTextList,propertyList') ->

         // Call the helper, which calls the eProperty drawer for each note in the group
         match (groupPropertiesHelper nList [] propertyList') with
         | Some(eTextList,propertyList'') ->
            Some((mTextList @ eTextList),propertyList'')

         | None -> None
      | None -> None

   | TupletNote(nList) ->

      // draw properties on each of the notes
      let rec tupletProperties (tNotes: Element List) (acc: string List) (pList: PropertyList) (isGrace: bool) : (string List * PropertyList) option =

         match tNotes with
         | [] -> Some(acc,pList)
         | h::t ->

            match (drawPropertiesElement h pList isGrace measureNumber) with
            | Some(newTList, newPList) ->
               tupletProperties t (acc @ newTList) newPList isGrace
            | None -> None


      match (tupletProperties nList [] propertyList isGrace) with
      | Some(totalTList, finalTPList) -> Some(totalTList, finalTPList)
      | None -> None

   // not a note
   | _ ->
      Some([""],propertyList)





(* helper for drawing properties of one measure
1) els is the Element list
2) text is the string list
3) propertyList is the record that describes the state of properties to be drawn
4) measureNumber is the measure number
RETURNS the string list and the new property list
*)
let rec drawPropertiesMeasures (els: Element List) (text: string List) (propertyList: PropertyList) (measureNumber: int) : (String List * PropertyList) option =
   match els with
   | [] ->
      Some(text,propertyList)
   | head::tail ->
      let graceNotes =
         match head.NoteInfo with
         | TupletNote(nList) -> nList.Head.GraceNotes
         | _ -> head.GraceNotes

      // First, draw properties on the grace notes
      let rec drawGraceProperties (grace: Element List) (t: string List) (pList: PropertyList) (measureNumber: int) : (String List * PropertyList) option =
         match grace with
         | [] -> Some(t,pList)
         | head::tail ->
            // for each grace note, call the element property drawer with isGrace set to true
            match (drawPropertiesElement head pList true measureNumber) with
            | Some(nText, newPList) ->
               drawGraceProperties tail (t @ nText) newPList measureNumber
            | None -> None


      match (drawGraceProperties graceNotes [] propertyList measureNumber) with

      | Some(nText, newPList) ->
         match (drawPropertiesElement head newPList false measureNumber) with
         | Some(newText, newPropertyList) ->
            drawPropertiesMeasures tail (text @ nText @ newText) newPropertyList measureNumber
         | None -> None

      | None -> None






(* driver for properties
1) ms is the list of SingleMeasures
2) text is the string list
3) propertyList is the record that describes the state of properties to be drawn
RETURNS the flattened string and the new property list
*)
let rec drawProperties (ms: SingleMeasure List) (text: string List) (propertyList: PropertyList) : (String * PropertyList) option =

   match ms with
   | [] ->
      let flattenedText = List.fold (fun acc elem -> acc + " " + elem) " " text
      Some(flattenedText,propertyList)
   | head::tail ->
      // properties for regular notes
      match (drawPropertiesMeasures head.Elements text propertyList head.MeasureNumber) with
      | Some(newText,newPropertyList) ->
         drawProperties tail newText newPropertyList
      | None -> None



(* add a tie at the end of a line if needed, and update the PropertyList
1) restOfLines is the rest of the lines after the current. Need it to check the next line
2) propertyList describes how to draw certain properties that depend on previous notes
*)
let checkEndTie (restOfLines: Line List) (propertyList: PropertyList) =

   let mapToList = propertyList.TieStart |> Map.toList

   let rec checkEndTieHelper l newPList text =
      match l with
      | [] -> Some(text,newPList)
      | head::tail ->
         match head with
         | (n,((x,y),p,g,b)) when x <> 0.0 && y <> 0.0 && b = true ->

            let lastX = 565.0
            let tieStub = string x + " " + string y + " " + string lastX + " " + string y + " 0 tie "

            // try and set the new TieStart, but if there are no more lines, error
            try
               // figure out the start of the next line
               let nextHead = restOfLines.Head
               let (nextStartX,nextStartY) = nextHead.Start
               // shift the x and y
               let newP = newPList.TieStart.Remove(n).Add(n,((nextStartX + 8.0,(nextStartY - 2.3) + (6.0 * ((float n) - 1.0))),p,g,true))
               let newPropertyList = { newPList with TieStart = newP }
               checkEndTieHelper tail newPropertyList (text + tieStub)
            with
            | _ ->
               printfn "Unended tie detected."
               None

         | _ ->
            checkEndTieHelper tail newPList text

   checkEndTieHelper mapToList propertyList ""





(* add a slur at the end of a line if needed, and update the PropertyList
1) restOfLines is the rest of the lines after the current. Need it to check the next line
2) propertyList describes how to draw certain properties that depend on previous notes
*)
let checkEndSlur (restOfLines: Line List) (propertyList: PropertyList) =
   match propertyList.SlurStart with
   // this means a slur ended in a previous line and needs to be extended
   | ((x,y),b,g) when x <> 0.0 && y <> 0.0 && b = true ->
      // draw a slur stub from where it began to the last barline
      let lastX = 565.0 //565 is where a line ends
      let slurCommand =
         match (lastX - x) with
         | num when num > 80.0 -> " slurlong "
         | num when num < 20.0 -> " slurshort "
         | _ -> " slur "
      let slurStub = string x + " " + string y + " " + string lastX + " " + string y + " " + slurCommand

      // try and set the new SlurStart, but if there are no more lines, error
      try
         // figure out the start of the next line
         let nextHead = restOfLines.Head
         let (nextStartX,nextStartY) = nextHead.Start
         let newPropertyList = { propertyList with SlurStart = ((nextStartX,nextStartY),true,false) }
         Some(slurStub,newPropertyList)

      with
      | _ ->
         printfn "Unended slur detected."
         None
   | _ ->
      Some("",propertyList)




(* add a palm mute at the end of a line if needed, and update the PropertyList
1) restOfLines is the rest of the lines after the current. Need it to check the next line
2) propertyList describes how to draw certain properties that depend on previous notes
*)
let checkEndMute (restOfLines: Line List) (propertyList: PropertyList) =
   match propertyList.MuteStart with
   // this means a mute ended in a previous line and needs to be extended
   | ((x,y),v) when x <> 0.0 && y <> 0.0 && v = true ->
      // draw a mute stub from where it began to the last barline
      let lastX = 565.0 //565 is where a line ends
      let muteCommand = " longpalmmute "
      let muteStub = string x + " " + string y + " " + string lastX + " " + string y + " " + muteCommand

      // try and set the new MuteStart, but if there are no more lines, error
      try
         // figure out the start of the next line
         let nextHead = restOfLines.Head
         let (nextStartX,nextStartY) = nextHead.Start
         let newPropertyList = { propertyList with MuteStart = ((nextStartX,nextStartY),true) }
         Some(muteStub,newPropertyList)

      with
      | _ ->
         printfn "Unended mute detected."
         None
   | _ ->
      Some("",propertyList)




(* add a hammer at the end of a line if needed, and update the PropertyList
1) restOfLines is the rest of the lines after the current. Need it to check the next line
2) propertyList describes how to draw certain properties that depend on previous notes
*)
let checkEndHammer (restOfLines: Line List) (propertyList: PropertyList) =

   let mapToList = propertyList.HammerStart |> Map.toList

   let rec checkEndHammerHelper l newPList (text: String) : (PropertyList * String) option =
      match l with
      | [] -> Some(newPList,text)
      | head::tail ->
         match head with
         | (n,((x,y),f,g,b)) when b = true ->

            // try and set the new HammerStart, but if there are no more lines, error
            try
               // figure out the start of the next line
               let nextHead = restOfLines.Head
               let (nextStartX,nextStartY) = nextHead.Start
               // shift the x and y
               let newP = newPList.HammerStart.Remove(n).Add(n,((nextStartX + 8.0,(nextStartY - 2.3) + (6.0 * ((float n) - 1.0))),f,g,true))
               let newPropertyList = { newPList with HammerStart = newP }

               let direction =
                  match g with
                  | true -> " hammerfirstgrace "
                  | false -> " hammer "

               let newText = text + " " + string x + " " + string y + " 565 " + string y + " " + string f + " " + string f + " " + direction

               checkEndHammerHelper tail newPropertyList newText
            with
            | _ ->
               printfn "Unended hammer detected."
               None

         | _ ->
            checkEndHammerHelper tail newPList text

   checkEndHammerHelper mapToList propertyList ""




(* push a slide to the next line if needed, and update property list
1) restOfLines is the rest of the lines after the current. Need it to check the next line
2) propertyList describes how to draw certain properties that depend on previous notes
*)
let checkEndSlide (restOfLines: Line List) (propertyList: PropertyList) =

   let mapToList = propertyList.SlideStart |> Map.toList

   let rec checkEndSlideHelper l newPList =
      match l with
      | [] -> Some(newPList)
      | head::tail ->
         match head with
         | (n,((x,y),f,g,b)) when b = true ->

            // try and set the new SlideStart, but if there are no more lines, error
            try
               // figure out the start of the next line
               let nextHead = restOfLines.Head
               let (nextStartX,nextStartY) = nextHead.Start
               // shift the x and y
               let newP = newPList.SlideStart.Remove(n).Add(n,((nextStartX + 8.0,(nextStartY - 2.3) + (6.0 * ((float n) - 1.0))),f,g,true))
               let newPropertyList = { newPList with SlideStart = newP }

               // Update the slide stubs so that the next slide knows to add the little stub at the end of the previous line
               let newSlideStubs = newPList.SlideStubs.Remove(n).Add(n,((x,y),f,g,b))

               let newerPropertyList = { newPropertyList with SlideStubs = newSlideStubs }

               checkEndSlideHelper tail newerPropertyList
            with
            | _ ->
               printfn "Unended slide detected."
               None

         | _ ->
            checkEndSlideHelper tail newPList

   checkEndSlideHelper mapToList propertyList
