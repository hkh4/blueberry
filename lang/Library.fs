module CS334

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
g = grace note
har = harmonic
sl = slide
si = slide in

*)
type Pitch =
| A | ASharp | AFlat | B | BSharp | BFlat | C | CSharp | CFlat | D | DSharp | DFlat | E | ESharp | EFlat | F | FSharp | FFlat | G | GSharp | GFlat

type Simple =
| Singlesimple of string * Pitch
| Restsimple

type Note =
| Simple | Complex | Group | Tuplet

type Type =

type Option =
| Type | Time | Key

type Expr =
| Option
| Measure of Note List





let expr, exprImpl = recparser()
exprImple := pseq (pchar 'a') pws1
