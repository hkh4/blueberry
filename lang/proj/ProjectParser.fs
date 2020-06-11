module ProjectParser

open System
open FParsec
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
sld = slide
sli = slide in
tie = tie
*)

type Pitch =
| A | ASharp | AFlat | ANat | B | BSharp | BFlat | BNat | C | CSharp | CFlat | CNat | D | DSharp | DFlat | DNat | E | ESharp | EFlat | ENat | F | FSharp | FFlat | FNat | G | GSharp | GFlat | GNat | NoPitch

type MultiProperty =
| Gra | Har | Stu | Std | Plu | Pld | Sls | Sle

type EitherProperty =
| Par | Sld | Sli | Tie

type Property =
| Multi of MultiProperty
| Either of EitherProperty

type RhythmNumber =
| X0 | X1 | X2 | X4 | X8 | X16 | X32 | X64

type Rhythm =
| R of RhythmNumber * int          //int is the number of dots
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

type Expr =
| ScoreOption of string * string
| Measure of int * Note List


// Generic types to avoid Value Restriction error
type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>


// HELPERS
let pstr s = pstring s
// Helper parse that parses one word
let word : Parser<_> = many1Satisfy (fun c -> c <> ' ' && c <> '\n' && c <> '\r' && c <> '\t') <??> "Expecting a word"
// Single space
let regularSpace : Parser<_> = pchar ' ' <??> "Expecting a space"
// Multiple spaces
let multipleSpaces : Parser<_> = manySatisfy (fun c -> c = ' ') <??> "Expecting whitespace"
let multipleSpaces1 = many1Satisfy (fun c -> c = ' ') <??> "Expecting whitespace"
// debugging
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
   fun stream ->
      //printfn "%A: Entering %s" stream.Position label
      let reply = p stream
      //printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
      reply



//**************** PARSE OPTIONS *******************

// After the '-', there needs to be a word followed
let optionWord_spaces1 = (word .>> multipleSpaces1) <??> "Option identifier followed by whitespace"

let optionIdentifier = pstr "-" >>. optionWord_spaces1 <??> "'-' followed by the option identifier. No spaces in between."
let singleOption = (optionIdentifier .>>. ((restOfLine false) .>> newline)) |>> ScoreOption
let option = (many singleOption) .>> spaces



//**************** PARSE MEASURES ******************

//pitches
let csharp = pstr "c#" >>% CSharp
let cflat = pstr "cb" >>% CFlat
let cnat = pstr "cn" >>% CNat
let dsharp = pstr "d#" >>% DSharp
let dflat = pstr "db" >>% DFlat
let dnat = pstr "dn" >>% DNat
let esharp = pstr "e#" >>% ESharp
let eflat = pstr "eb" >>% EFlat
let enat = pstr "en" >>% ENat
let fsharp = pstr "f#" >>% FSharp
let fflat = pstr "fb" >>% FFlat
let fnat = pstr "fn" >>% FNat
let gsharp = pstr "g#" >>% GSharp
let gflat = pstr "gb" >>% GFlat
let gnat = pstr "gn" >>% GNat
let asharp = pstr "a#" >>% ASharp
let aflat = pstr "ab" >>% AFlat
let anat = pstr "an" >>% ANat
let bsharp = pstr "b#" >>% BSharp
let bflat = pstr "bb" >>% BFlat
let bnat = pstr "bn" >>% BNat
let a = pchar 'a' >>% A
let b = pchar 'b' >>% B
let c = pchar 'c' >>% C
let d = pchar 'd' >>% D
let e = pchar 'e' >>% E
let f = pchar 'f' >>% F
let g = pchar 'g' >>% G
let noPitch = pchar 'x' >>% NoPitch

let pitch = attempt csharp <|> cflat <|> cnat <|> dsharp <|> dflat <|> dnat <|> esharp <|> eflat <|> enat <|> fsharp <|> fflat <|> fnat <|> gsharp <|> gflat <|> gnat <|> asharp <|> aflat <|> anat <|> bsharp <|> bflat <|> bnat <|> a <|> b <|> c <|> d <|> e <|> f <|> g <|> noPitch <??> "A valid pitch: 'a', 'a#', 'ab', 'an', 'b', 'b#', 'bb', 'bn', 'c', 'c#', 'cb', 'cn', 'd', 'd#', 'db', 'dn', 'e', 'e#', 'eb', 'en', 'f', 'f#', 'fb', 'fn', 'g', 'g#', 'gb', 'gn' or 'x'"

//properties
let sls = pstr "sls" >>% Sls
let sle = pstr "sle" >>% Sle
let stu = pstr "stu" >>% Stu
let std = pstr "std" >>% Std
let plu = pstr "plu" >>% Plu
let pld = pstr "pld" >>% Pld
let har = pstr "har" >>% Har
let gra = pstr "gra" >>% Gra
let sld = pstr "sld" >>% Sld
let sli = pstr "sli" >>% Sli
let par = pstr "par" >>% Par
let tie = pstr "tie" >>% Tie



// Properties
let eitherProperty = pchar '/' >>. (par <|> sld <|> sli <|> tie)

let multiProperty = pchar '/' >>. (stu <|> std <|> plu <|> pld <|> har <|> gra <|> sls <|> sle)

let eitherProperties = many eitherProperty

let multiProperties = many multiProperty

let anyProperty = pchar '/' >>. ((par |>> Either) <|> (sld |>> Either) <|> (sli |>> Either) <|> (tie |>> Either) <|> (stu |>> Multi) <|> (std |>> Multi) <|> (plu |>> Multi) <|> (pld |>> Multi) <|> (har |>> Multi) <|> (gra |>> Multi) <|> (sls |>> Multi) <|> (sle |>> Multi)) <!> "anyproperty"
let anyProperties = many anyProperty <??> "property" <!> "anyProperties"


// Rhythms
let x64 = pstr "64" >>% X64
let x32 = pstr "32" >>% X32
let x16 = pstr "16" >>% X16
let x8 = pstr "8" >>% X8
let x4 = pstr "4" >>% X4
let x2 = pstr "2" >>% X2
let x1 = pstr "1" >>% X1
let x0 = pstr "0" >>% X0

let dot = many (pchar '.') |>> (fun list -> list.Length)

let rhythm = ((x64 <|> x32 <|> x16 <|> x8 <|> x4 <|> x2 <|> x1 <|> x0) .>>. dot) |>> R <??> "Rhythm with the form (int)(dots) where int is 0, 1, 2, 4, 8, 16, 32, or 64 and dots are a sequence of '.'"


// After the measure number and after each note, there should be a bunch of spaces followed by a newline
let spacesAndNewLine = (many (pchar ' ')) .>> newline


// for the measure number
let int32 = pint32 <??> "An integer"
let measureNumber = int32 .>> ((pchar ':') .>> spacesAndNewLine) <!> "measurenumber"
let stringNum = int32




/// SIMPLE ///
// simplesingle
let singlesimple = tuple3 stringNum pitch anyProperties |>> SingleSimple <!> "singlesimple"

// restsimple
let restsimple = pchar 'r' >>% RestSimple <!> "restsimple"

// simple
let simple = singlesimple <|> restsimple |>> Simple <!> "simple"

/// COMPLEX ///

// singlecomplex
let singlecomplex = tuple4 stringNum pitch rhythm anyProperties |>> SingleComplex <!> "singlecomplex"

// restcomplex
let restcomplex = (pchar 'r') >>. rhythm |>> RestComplex <!> "restcomplex"

// complex
let complex = singlecomplex <|> restcomplex |>> Complex <!> "complex"

/// GROUP ///
let oneNoteInGroup = tuple3 stringNum pitch eitherProperties |>> GS <!> "one note in a group"

let multipleNotes = sepBy oneNoteInGroup spaces1 <!> "multiplenotes"

let notesWithParens = between (pchar '(') (pchar ')') multipleNotes <!> "noteswithparens"

let groupsimple = notesWithParens .>>. multiProperties |>> GSimple <!> "groupsimple"

let groupcomplex = tuple3 notesWithParens rhythm multiProperties |>> GComplex <!> "groupcomplex"

let group = attempt groupcomplex <|> attempt groupsimple |>> Group <!> "group"



let note = spaces1 >>. (attempt complex <|> attempt simple <|> attempt group) .>> spacesAndNewLine <??> "A note or a rest" <!> "note"

let measure1 = measureNumber .>>. (many1 note) |>> Measure <!> "measure"

let expr = (option .>>. (many measure1)) .>> spaces <!> "expr"

let grammar = expr .>> eof



































//
