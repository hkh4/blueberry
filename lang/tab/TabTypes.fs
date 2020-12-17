module TabTypes

open System
open FParsec

// fret, start string, end string
type barre =
| Barre of int * int * int
| EmptyBarre

// fret, string
type spot = Spot of int * int

type TabExpr =
| TabOption of string * string
| Chart of barre * spot list
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
let chartStart = (72.0, 610.0)

// location of the first start on the second page and onwards
let chartStart2 = (72.0, 650.0)

// max number of charts per page
let chartMax = 30

// horizontal spacing
let chartXSkip = 82.0

// vertical spacing
let chartYSkip = 100.0






















//
