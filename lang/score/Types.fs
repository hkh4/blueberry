module Types

open System
open FParsec

//****** Parser types

(*
Notes:
sls = slur start
sle = slur end
stu = strum up
std = strum down
par = parens
plu = pluck up
pld = pluck down
gra = grace note
har = harmonic
sld = slide down
slu = slide up
sli = slide
tie = tie
^ = up fret
ham = hammer
plm = palm mute
pl1 = start of long palm mute
pl2 = end of long palm mute
*)

type Pitch =
| A | ASharp | AFlat | ANat | B | BSharp | BFlat | BNat | C | CSharp | CFlat | CNat | D | DSharp | DFlat | DNat | E | ESharp | EFlat | ENat | F | FSharp | FFlat | FNat | G | GSharp | GFlat | GNat | NoPitch

type MultiProperty =
| Gra | Stu | Std | Plu | Pld | Sls | Sle | Plm | Pl1 | Pl2

type EitherProperty =
| Par | Sld | Sli | Slu | Tie | Har | Upf | Ham

type Property =
| Multi of MultiProperty
| Either of EitherProperty

type RhythmNumber =
| X0
| X1
| X2
| X4
| X8
| X16
| X32
| X64

type Rhythm =
| R of RhythmNumber * int     //int is the number of dots
| Other

type simple =
| SingleSimple of int * Pitch * Property List
| RestSimple

type complex =
| SingleComplex of int * Pitch * Rhythm * Property List
| RestComplex of Rhythm

type GroupSimple = GS of int * Pitch * EitherProperty List

type group =
| GSimple of GroupSimple List * MultiProperty List
| GComplex of GroupSimple List * Rhythm * MultiProperty List

type Note =
| Simple of simple
| Complex of complex
| Group of group
| Tuplet of Note List * Rhythm
| Comment of String
| HiddenComment


type Expr =
| ScoreOption of string * string
| Measure of int * Note List


// ****** Interpreter types

type optionsRecord = {
   Time: int * int
   Key: string
   Capo: int
   Title: string
   Composer: string
   Tuning: string
   TuningNumbers: int List
}

type singleNote =
| NormalGuitarNote of int * Pitch * int * EitherProperty List
| X of int * EitherProperty List

// Types of "notes"
type Notehead =
| SingleNote of singleNote * MultiProperty List
| GroupNote of singleNote List * MultiProperty List
| TupletNote of Element List
| Rest
| Barline
| Empty
| Buffer
| TimeChange of int * int
and Element = {
   NoteInfo: Notehead
   Duration: Rhythm
   Start: float
   Width: float
   LastNote: int
   Location: float * float
   GraceNotes: Element List
   Comments: String
}

type SingleMeasure = {
   Key: string
   Time: int * int
   Capo: int
   MeasureNumber: int
   Elements: Element List
   Width: float
   Changes: {| Time: bool; Key: bool; Capo: bool |}
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

type PropertyList = {
   // (x,y),fret,grace note,valid
   SlurStart: (float * float) * bool * bool
   // (x,y),valid
   MuteStart: (float * float) * bool
   // <string,(x,y),fret,grace,valid>
   TieStart: Map<int,(float * float) * int * bool * bool>
   // <string: (x,y),fret,grace,valid>
   SlideStart: Map<int,(float * float) * int * bool * bool>
   // If a stub is needed for a slide on the previous line
   SlideStubs: Map<int,(float * float) * int * bool * bool>
   // <string: (x,y), fret, grace, valid>
   HammerStart: Map<int,(float * float) * int * bool * bool>
}

///////// Useful Global Variables And Functions /////////

// Rhythms ordered in an array in order to be used to do rhythm math
let arrayOfRhythms = [|X1;X2;X4;X8;X16;X32;X64|]

// Widths of different rhythms
let widthOfRhythms =
   Map.empty.
      Add(R(X0,0),25.0).
      Add(R(X1,3),73.0).
      Add(R(X1,2),63.0).
      Add(R(X1,1),53.0).
      Add(R(X1,0),43.5).
      Add(R(X2,3),39.5).
      Add(R(X2,2),36.5).
      Add(R(X2,1),33.5).
      Add(R(X2,0),29.5).
      Add(R(X4,3),27.5).
      Add(R(X4,2),25.0).
      Add(R(X4,1),23.0).
      Add(R(X4,0),20.0).
      Add(R(X8,3),18.5).
      Add(R(X8,2),17.0).
      Add(R(X8,1),15.5).
      Add(R(X8,0),14.0).
      Add(R(X16,2),12.6).
      Add(R(X16,1),11.6).
      Add(R(X16,0),10.5).
      Add(R(X32,2),9.5).
      Add(R(X32,1),9.1).
      Add(R(X32,0),8.7).
      Add(R(X64,2),8.3).
      Add(R(X64,1),7.9).
      Add(R(X64,0),7.5)

// widths of grace notes
let widthOfGraceRhythms =
   Map.empty.
      Add(Other,0.0).
      Add(R(X0,0),0.0). // should never happen
      Add(R(X1,3),22.5).
      Add(R(X1,2),19.5).
      Add(R(X1,1),17.5).
      Add(R(X1,0),15.5).
      Add(R(X2,3),13.5).
      Add(R(X2,2),12.5).
      Add(R(X2,1),11.5).
      Add(R(X2,0),10.5).
      Add(R(X4,3),9.5).
      Add(R(X4,2),9.0).
      Add(R(X4,1),8.5).
      Add(R(X4,0),8.0).
      Add(R(X8,3),7.5).
      Add(R(X8,2),7.1).
      Add(R(X8,1),6.8).
      Add(R(X8,0),6.5).
      Add(R(X16,2),6.2).
      Add(R(X16,1),6.0).
      Add(R(X16,0),5.8).
      Add(R(X32,2),5.7).
      Add(R(X32,1),5.6).
      Add(R(X32,0),5.5).
      Add(R(X64,2),5.5).
      Add(R(X64,1),5.5).
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
      Capo = 0;
      MeasureNumber = 0;
      Elements = [{ NoteInfo = Empty; Comments = ""; Start = 0.0; Duration = Other; Width = 5.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = [] };{ NoteInfo = Rest; Comments = ""; Duration = R(X0,0); Start = 1.0; Width = 30.0; LastNote = 1; Location = (0.0,0.0); GraceNotes = [] };{ NoteInfo = Barline; Comments = ""; Start = 0.0; Duration = Other; Width = 0.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = [] }];
      Width = 35.0;
      Changes = {| Time = false; Key = false; Capo = false; |}
   }


let emptyElement = { NoteInfo = Empty; Duration = Other; Start = 0.0; Width = 0.0; LastNote = 0; Location = (0.0,0.0); GraceNotes = []; Comments = "" }


// Template for drawing dots
let dotTemplate (x: float) (y: float) (isGrace: bool) : string =
   match isGrace with
   // not a grace note
   | false ->
      " 1 setlinecap 1.4 setlinewidth " + string x + " " + string y + " moveto 0 0 rlineto stroke 0 setlinecap "
   // grace note
   | true ->
      " 1 setlinecap 1.1 setlinewidth " + string x + " " + string y + " moveto 0 0 rlineto stroke 0 setlinecap "


// Template for drawing flags
let drawFlags (x: float) (y: float) (r: Rhythm) (isGrace: bool) : string List =
   match isGrace with
   // not a grace note
   | false ->
      match r with
      | R(X8,n) -> ["0.9 setlinewidth 1 1 1 setrgbcolor " + string (x + 2.0) + " " + string (y + 41.0) + " moveto 0 1 rlineto stroke 0 0 0 setrgbcolor ";string (x + 1.65) + " " + string (y + 40.0) + " drawFlag "]
      // for 16th notes and shorter, need to extend the stem as well
      | R(X16,n) -> ["0.7 setlinewidth " + string (x + 2.0) + " " + string (y + 42.0) + " moveto 0 3 rlineto stroke ";string (x + 1.65) + " " + string (y + 40.0) + " drawFlag ";string (x + 1.65) + " " + string (y + 44.0) + " drawFlag "]
      | R(X32,n) -> ["0.7 setlinewidth " + string (x + 2.0) + " " + string (y + 42.0) + " moveto 0 7 rlineto stroke ";string (x + 1.65) + " " + string (y + 40.0) + " drawFlag ";string (x + 1.65) + " " + string (y + 44.0) + " drawFlag ";string (x + 1.65) + " " + string (y + 48.0) + " drawFlag "]
      | R(X64,n) -> ["0.7 setlinewidth " + string (x + 2.0) + " " + string (y + 42.0) + " moveto 0 11 rlineto stroke ";string (x + 1.65) + " " + string (y + 40.0) + " drawFlag ";string (x + 1.65) + " " + string (y + 44.0) + " drawFlag ";string (x + 1.65) + " " + string (y + 48.0) + " drawFlag ";string (x + 1.65) + " " + string (y + 52.0) + " drawFlag "]
      | _ -> [""]
   // grace note
   | true ->
      match r with
      | R(X8,n) -> ["0.5 setlinewidth 1 1 1 setrgbcolor " + string (x + 1.25) + " " + string (y + 39.0) + " moveto 0 1 rlineto stroke 0 0 0 setrgbcolor ";string (x + 1.0) + " " + string (y + 38.5) + " drawFlagGrace "]
      // for 16th notes and shorter, need to extend the stem as well
      | R(X16,n) -> [string (x + 1.0) + " " + string (y + 37.0) + " drawFlagGrace ";string (x + 1.0) + " " + string (y + 39.2) + " drawFlagGrace "]
      | R(X32,n) -> ["0.5 setlinewidth " + string (x + 1.25) + " " + string (y + 39.0) + " moveto 0 0 rlineto stroke ";string (x + 1.0) + " " + string (y + 37.0) + " drawFlagGrace ";string (x + 1.0) + " " + string (y + 39.2) + " drawFlagGrace ";string (x + 1.0) + " " + string (y + 41.4) + " drawFlagGrace "]
      | R(X64,n) -> ["0.5 setlinewidth " + string (x + 1.25) + " " + string (y + 39.0) + " moveto 0 0 rlineto stroke ";string (x + 1.0) + " " + string (y + 37.0) + " drawFlagGrace ";string (x + 1.0) + " " + string (y + 39.2) + " drawFlagGrace ";string (x + 1.0) + " " + string (y + 41.4) + " drawFlagGrace ";string (x + 1.0) + " " + string (y + 43.3) + " drawFlagGrace "]
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


// Buffer for beaming grace notes and for tuplets
let bufferElement = { NoteInfo = Buffer; Comments = ""; Duration = Other; Start = 0.0; Width = 0.0; LastNote = 0; Location = (0.0,0.0);  GraceNotes = [] }







//
