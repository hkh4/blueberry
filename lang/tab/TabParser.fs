module TabParser

open System
open TabTypes
open FParsec
open ParserHelpers


//**************** PARSE OPTIONS *******************

// helpful constants
let _0 = int '0'

// After the '-', there needs to be a word followed
let optionWord_spaces1 = (word .>> multipleSpaces1) <??> "Option identifier followed by whitespace"

let optionIdentifier = pstr "-" >>. optionWord_spaces1 <??> "'-' followed by the option identifier. No spaces in between."
let singleOption = (optionIdentifier .>>. ((restOfLine false) .>> (newline .>>. spaces))) |>> TabOption
let option = (many singleOption) .>> spaces

// Chord charts
let stringsBody = digit .>>. (pstr "-" >>. digit) <!> "stringsBody"
let strings = pstr "strings:" >>. (spaces >>. stringsBody) <!> "strings"
let fret =  pstr "fret:" >>. (spaces >>. digit) <!> "fret"
let barreInside = ((spaces >>. fret) .>>. (spaces >>. strings)) .>> spaces <!> "barreInside"
let barre = pstr "barre:" >>. (spaces >>. (between (pstr "[") (pstr "]") barreInside)) |>> (fun (a,(b,c)) -> (int a - _0, int b - _0, int c - _0)) |>> Barre <!> "barre"

let chartBody = barre <!> "chartBody"

let chart : Parser<_> = between (pstr "[") (pstr "]") (between spaces spaces chartBody) <!> "chart"

let tabExpr = (option .>>. chart) .>> spaces <!> "tabExpr"

let tabGrammar : Parser<_> = tabExpr .>> eof <!> "tabGrammar"
