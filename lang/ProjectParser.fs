module ProjectParser
 
open System
open Parser
(*
<expr>             ::= <option>
                     | <measure>
<option>           ::= <type>
                     | <time>
                     | <key>
<key>              ::= c | cm | c# | c#m| cb | d | dm | db | d#m | e | em | eb | ebm | f | fm | f# | f#m | g | gm | g#m | gb | a | am | a#m | ab | abm | b | bm | bb | bbm
<time>             ::= <num> / <num>
<num>              ::= x ∈ ℕ
<type>             ::= tab
<measure>          ::= <note>+
<note>             ::= <simple>
                     | <complex>
                     | <group>
                     | <tuplet>
<simple>           ::= <singlesimple>
                     | <restsimple>
<complex>          ::= <singlecomplex>
                     | <restcomplex>
<singlesimple>     ::= <string><pitch><property>*
<restsimple>       ::= r
<singlecomplex>    ::= <string><pitch><rhythm><property>*
<restcomplex>      ::= r<rhythm>
<group>            ::= (<singlesimple>+)
                     | (<singlesimple>+)<rhythm>
<tuplet>           ::= t<num>o<num> {<simple>+}
<string>           ::= 1 | 2 | 3 | 4 | 5 | 6
<pitch>            ::= c | c# | cb | d | d# | db | e | e# | eb | f | f# | fb | g | g# | gb | a | a# | ab | b | b# | bb
<rhythm>           ::= <rhythmnumber><dot>*
<rhythmnumber      ::= 1 | 2 | 4 | 8 | 16 | 32 | 64 | 128 | 256
<dot>              ::= .
<property>         ::= /sls | /sle | /stu | /std | /p | /plu | /pld | /g | /har | /sl | /si


Notes:
sls = slur start
sle = slur end
stu = strum up
std = strum down
p = parens
plu = pluck up
pld = pluck down
gr = grace note
har = harmonic
sl = slide
si = slide in

*)
type Pitch =
| A | ASharp | AFlat | ANat | B | BSharp | BFlat | BNat | C | CSharp | CFlat | CNat | D | DSharp | DFlat | DNat | E | ESharp | EFlat | ENat | F | FSharp | FFlat | FNat | G | GSharp | GFlat | GNat

type Property = | Gr | Sls | Sle | Stu | Std | P | Plu | Pld | Har | Sl | Si

type simple =
| SingleSimple of int * Pitch * Property List
| RestSimple

type RhythmNumber =
| X1 | X2 | X4 | X8 | X16 | X32 | X64

type Rhythm =
| R of RhythmNumber * int          //int is the number of dots
| Other

type complex =
| SingleComplex of int * Pitch * Rhythm * Property List
| RestComplex of Rhythm

type Note =
| Simple of simple
| Complex of complex

type Expr =
| ScoreOption of string * string
| Measure of int * Note List


// Helpers
let pword = pmany1 (psat (fun c -> (c <> ' ') && (c <> '\n'))) |>> stringify <!> "word"
let pdigit2 = pmany1 pdigit |>> stringify |>> int <!> "pdigit2"

// ****************  PARSE OPTIONS ********************

// '-' then word, then space, then another word, then space
let typeoption = pseq (pleft pword pws1 ) pword (fun (a,b) -> (a,b)) <!> "typeoption"
let singleOption = pbetween (pchar '-') pws1 typeoption |>> ScoreOption <!> "singleOption"
let option = pleft (pmany0 singleOption) pws0 <!> "option"

// **************** PARSE MEASURES ********************

// start with a measure number and then :
let measureNumber = pleft pdigit2 (pchar ':') <!> "Measure number"

///////////// NOTE ///////////////

//pitches
let csharp = pstr "c#" |>> (fun _ -> CSharp) <!> "csharp"
let cflat = pstr "cb" |>> (fun _ -> CFlat) <!> "cflat"
let cnat = pstr "cn" |>> (fun _ -> CNat) <!> "cnat"
let dsharp = pstr "d#" |>> (fun _ -> DSharp) <!> "dsharp"
let dflat = pstr "db" |>> (fun _ -> DFlat) <!> "dflat"
let dnat = pstr "dn" |>> (fun _ -> DNat) <!> "dnat"
let esharp = pstr "e#" |>> (fun _ -> ESharp) <!> "esharp"
let eflat = pstr "eb" |>> (fun _ -> EFlat) <!> "eflat"
let enat = pstr "en" |>> (fun _ -> ENat) <!> "enat"
let fsharp = pstr "f#" |>> (fun _ -> FSharp) <!> "fsharp"
let fflat = pstr "fb" |>> (fun _ -> FFlat) <!> "fflat"
let fnat = pstr "fn" |>> (fun _ -> FNat) <!> "fnat"
let gsharp = pstr "g#" |>> (fun _ -> GSharp) <!> "gsharp"
let gflat = pstr "gb" |>> (fun _ -> GFlat) <!> "gflat"
let gnat = pstr "gn" |>> (fun _ -> GNat) <!> "gnat"
let asharp = pstr "a#" |>> (fun _ -> ASharp) <!> "asharp"
let aflat = pstr "ab" |>> (fun _ -> AFlat) <!> "aflat"
let anat = pstr "an" |>> (fun _ -> ANat) <!> "anat"
let bsharp = pstr "b#" |>> (fun _ -> BSharp) <!> "bsharp"
let bflat = pstr "bb" |>> (fun _ -> BFlat) <!> "bflat"
let bnat = pstr "bn" |>> (fun _ -> BNat) <!> "bnat"
let a = pchar 'a' |>> (fun _ -> A) <!> "a"
let b = pchar 'b' |>> (fun _ -> B) <!> "b"
let c = pchar 'c' |>> (fun _ -> C) <!> "c"
let d = pchar 'd' |>> (fun _ -> D) <!> "d"
let e = pchar 'e' |>> (fun _ -> E) <!> "e"
let f = pchar 'f' |>> (fun _ -> F) <!> "f"
let g = pchar 'g' |>> (fun _ -> G) <!> "g"

let pitch = csharp <|> cflat <|> cnat <|> dsharp <|> dflat <|> dnat <|> esharp <|> eflat <|> enat <|> fsharp <|> fflat <|> fnat <|> gsharp <|> gflat <|> gnat <|> asharp <|> aflat <|> anat <|> bsharp <|> bflat <|> bnat <|> a <|> b <|> c <|> d <|> e <|> f <|> g <!> "pitch"

//properties
let sls = pstr "sls" |>> (fun _ -> Sls) <!> "sls"
let sle = pstr "sle" |>> (fun _ -> Sle) <!> "sle"
let stu = pstr "stu" |>> (fun _ -> Stu) <!> "stu"
let std = pstr "std" |>> (fun _ -> Std) <!> "std"
let plu = pstr "plu" |>> (fun _ -> Plu) <!> "plu"
let pld = pstr "pld" |>> (fun _ -> Pld) <!> "pld"
let har = pstr "har" |>> (fun _ -> Har) <!> "har"
let gr = pstr "gr" |>> (fun _ -> Gr) <!> "gr"
let sl = pstr "sl" |>> (fun _ -> Sl) <!> "sl"
let si = pstr "si" |>> (fun _ -> Si) <!> "si"
let p = pstr "p" |>> (fun _ -> P) <!> "p"

let property = pright (pchar '/') (sls <|> sle <|> stu <|> std <|> plu <|> pld <|> har <|> gr <|> sl <|> si <|> p) <!> "property"
let properties = pmany0 property <!> "properties"

//rhythms
let x64 = pstr "64" |>> (fun _ -> X64) <!> "64"
let x32 = pstr "32" |>> (fun _ -> X32) <!> "32"
let x16 = pstr "16" |>> (fun _ -> X16) <!> "16"
let x8 = pstr "8" |>> (fun _ -> X8) <!> "8"
let x4 = pstr "4" |>> (fun _ -> X4) <!> "4"
let x2 = pstr "2" |>> (fun _ -> X2) <!> "2"
let x1 = pstr "1" |>> (fun _ -> X1) <!> "1"

let dot = pmany0 (pchar '.') |>> (fun list -> list.Length) <!> "dot"

let rhythm = pseq (x64 <|> x32 <|> x16 <|> x8 <|> x4 <|> x2 <|> x1) dot (fun (a,b) -> (a,b)) |>> R <!> "Rhythm"

///// SIMPLE //////

//SINGLESIMPLE
let singlesimple = pseq pdigit2 (pseq pitch properties (fun (a,b) -> (a,b))) (fun (a,(b,c)) -> (a,b,c)) |>> SingleSimple <!> "singlesimple"

//RESTSIMPLE
let restsimple = pchar 'r' |>> (fun _ -> RestSimple) <!> "restsimple"

// Simple
let simple = singlesimple <|> restsimple |>> Simple <!> "simple"

///// COMPLEX //////

//SINGLECOMPLEX
let singlecomplex = pseq pdigit2 (pseq pitch (pseq rhythm properties (fun (a,b) -> (a,b))) (fun (a,(b,c)) -> (a,b,c))) (fun (a,(b,c,d)) -> (a,b,c,d)) |>> SingleComplex <!> "singlecomplex"

//RESTCOMPLEX
let restcomplex = pright (pchar 'r') rhythm |>> RestComplex <!> "restcomplex"

// Complex
let complex = singlecomplex <|> restcomplex |>> Complex <!> "complex"

// Note
let note = pright pws1 (complex <|> simple) <!> "note"

// Measure
let measure1 = pseq measureNumber (pmany1 note) (fun (a,b) -> (a,b)) |>> Measure <!> "measure!"


let expr = pseq option (pmany0 (pleft measure1 pws1)) (fun (a,b) -> (a,b)) <!> "expr"

let grammar = pleft expr (pleft pws0 peof) <!> "grammar"

let parse input =
   let input' = prepare input
   match grammar input' with
   | Success(res,_) -> Some res
   | Failure(pos,rule) ->
      printfn "Invalid expression"
      let msg = sprintf "Cannot parse input at pos %d in rule '%s':" pos rule
      let diag = diagnosticMessage 20 pos input msg
      printf "%s" diag
      None
































//
