module TabTypes

open System
open FParsec

// fret, start string, end string
type Barre = int * int * int

// fret, string
type Spot = int * int

type TabExpr =
| TabOption of string * string
| Chord of Barre * Spot list
| Music of string
