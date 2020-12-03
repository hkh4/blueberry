module ScoreParser

open System
open FParsec
open Types
open ParserHelpers

//**************** PARSE OPTIONS *******************

// After the '-', there needs to be a word followed
let optionWord_spaces1 = (word .>> multipleSpaces1) <??> "Option identifier followed by whitespace"

let optionIdentifier = pstr "-" >>. optionWord_spaces1 <??> "'-' followed by the option identifier. No spaces in between."
let singleOption = (optionIdentifier .>>. ((restOfLine false) .>> (newline .>>. spaces))) |>> ScoreOption
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
let slu = pstr "slu" >>% Slu
let par = pstr "par" >>% Par
let tie = pstr "tie" >>% Tie
let upf = pstr "^" >>% Upf
let ham = pstr "ham" >>% Ham
let plm = pstr "plm" >>% Plm
let pl1 = pstr "pl1" >>% Pl1
let pl2 = pstr "pl2" >>% Pl2



// Properties
let slash = pchar '/' <!> "slash"

let eitherProperty = pchar '/' >>. (par <|> sld <|> sli <|> tie <|> slu <|> har <|> upf <|> ham)

let multiProperty = slash >>. (stu <|> std <|> plu <|> pld <|> gra <|> sls <|> sle <|> plm <|> pl1 <|> pl2) <!> "multiProperty" <??> "stu (strum up), std (strum down), plu (pluck up), pld (pluck down), har (harmonic), gra (grace note), plm (palm mute), pl1 (long palm mute start), pl2 (long palm mute end), sls (slur start), or sle (slur end). Any other properties should be included with each individual note inside the parentheses"

let eitherProperties = many eitherProperty

let multiProperties = many multiProperty <!> "multiProperties"

let anyProperty = pchar '/' >>. ((par |>> Either) <|> (ham |>> Either) <|> (slu |>> Either) <|> (sld |>> Either) <|> (sli |>> Either)  <|> (har |>> Either) <|> (tie |>> Either) <|> (upf |>> Either) <|> (stu |>> Multi) <|> (std |>> Multi) <|> (plu |>> Multi) <|> (pld |>> Multi) <|> (gra |>> Multi) <|> (plm |>> Multi) <|> (pl1 |>> Multi) <|> (pl2 |>> Multi) <|> (sls |>> Multi) <|> (sle |>> Multi)) <!> "anyproperty"
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

let rhythm = ((x64 <|> x32 <|> x16 <|> x8 <|> x4 <|> x2 <|> x1 <|> x0) .>>. dot) |>> R <!> "rhythm" <??> "Rhythm with the form (int)(dots) where int is 0, 1, 2, 4, 8, 16, 32, or 64 and dots are a sequence of '.'"


// After the measure number and after each note, there should be a bunch of spaces followed by a newline
let emptySpaces = many (pchar ' ')
let emptySpaces1 : Parser<_> = many1 (pchar ' ')
let spacesAndNewLine = emptySpaces .>> newline
let emptyLines = many spacesAndNewLine <!> "empty lines"


// for the measure number
let int32 = pint32 <??> "An integer"
let measureNumber = int32 .>> ((pchar ':') .>> spacesAndNewLine) <!> "measurenumber"
let stringNum = int32



/// RESTS ///
// restsimple
let restsimple = pchar 'r' >>% RestSimple |>> Simple <!> "restsimple"

// restcomplex
let restcomplex = (pchar 'r') >>? rhythm |>> RestComplex |>> Complex <!> "restcomplex"

let anyRest = restcomplex <|> restsimple



/// SINGLE NOTES ///

let noteTemplate = stringNum .>>. pitch <!> "note template"

// simplesingle
let singlesimple = noteTemplate .>>. anyProperties |>> (fun ((a,b),c) -> (a,b,c)) |>> SingleSimple |>> Simple <!> "singlesimple"

// singlecomplex
let singlecomplex = noteTemplate .>>.? (rhythm .>>. anyProperties) |>> (fun ((a,b),(c,d)) -> (a,b,c,d)) |>> SingleComplex |>> Complex <!> "singlecomplex"


let singleNote = singlecomplex <|> singlesimple





/// GROUP ///
let oneNoteInGroup = tuple3 stringNum pitch eitherProperties |>> GS <!> "one note in a group"

let multipleNotes = sepBy oneNoteInGroup spaces1 <!> "multiplenotes"

let notesWithParens = between (pchar '(') (pchar ')') multipleNotes <!> "noteswithparens"

let groupsimple = notesWithParens .>>. multiProperties |>> GSimple <!> "groupsimple"

let groupcomplex = notesWithParens .>>.? (rhythm .>>. multiProperties) |>> (fun (a,(b,c)) -> (a,b,c)) |>> GComplex <!> "groupcomplex"

let group = groupcomplex <|> groupsimple |>> Group <!> "group"


/// TUPLETS ///
let noteNoNL = (anyRest <|> group <|> singleNote) <??> "A note or a rest" <!> "note-nonewline"

let tupletBody = sepBy1 noteNoNL spaces1 <!> "tuplet body"

let tuplet = (between (pstr "<") (pstr ">") tupletBody) .>>. rhythm |>> Tuplet <??> "Tuplet" <!> "tuplet"


/// COMMENT ///

let commentBody = charsTillString "$" false 1000

let comment = (between (pstr "$") (pstr "$") commentBody) |>> Comment <??> "Comment" <!> "comment"

let hiddenCommentBody = charsTillString "%" false 1000 <!> "hiddenBody"

let hiddenCommentNoType = sepEndBy (between (pstr "%") (pstr "%") hiddenCommentBody) spaces1 <!> "hiddenNOType"

let hiddenComment = (between (pstr "%") (pstr "%") hiddenCommentBody) >>% HiddenComment <!> "hiddenComment"




let note : Parser<_> = spaces1 >>? (comment <|> anyRest <|> hiddenComment <|> group <|> singleNote <|> tuplet) .>> spacesAndNewLine <??> "A note or a rest" <!> "note"

let noteWithOptionalSpaces = note .>> optional (attempt emptyLines)

let measure1 = hiddenCommentNoType >>. (measureNumber .>>. (many1 noteWithOptionalSpaces) .>> spaces) |>> Measure <!> "measure"

let expr = (option .>>. (many (measure1 <|> singleOption))) .>> spaces <!> "expr"

let grammar = expr .>> eof



































//
