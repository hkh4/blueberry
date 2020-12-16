module TabTypes

open System
open FParsec

// fret, start string, end string
type barre = Barre of int * int * int

// fret, string
type spot = Spot of int * int

type TabExpr =
| TabOption of string * string
| Chord of barre * spot list
| Music of string

type optionsRecord = {
   Key: string
   Capo: int
   Title: string
   Composer: string
}

let defaultOptionsRecord = {
   Key = "c";
   Capo = 0;
   Title = "untitled";
   Composer = "unknown";
}
