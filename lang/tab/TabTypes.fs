module TabTypes

open System
open FParsec

// fret, start string, end string
type barre =
| Barre of int * int * int
| EmptyBarre


type spot =
| Spot of int * int // string, fret
| XSpot of int // just the string number

type TabExpr =
| TabOption of string * string
| Chart of string * barre list * spot list
| Music of string

type optionsRecord = {
   Key: string
   Capo: int
   Title: string
   Composer: string
}



//*************************** CONSTANTS ****************************

let defaultOptionsRecord = {
   Key = "c";
   Capo = 0;
   Title = "untitled";
   Composer = "unknown";
}


// **** Chart constants

// location of the first start
let chartStartX = 72.0
let chartStartY = 610.0

// location of the first start on the second page and onwards
let chartStart2X = 72.0
let chartStart2Y = 650.0

// max number of charts per page
let chartLineMax = 6
let chartPageMax = 30

// horizontal spacing
let chartXSkip = 82.0

// vertical spacing
let chartYSkip = 100.0






















//
