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

// options
let optionIdentifier = pstr "-" >>. optionWord_spaces1 <??> "'-' followed by the option identifier. No spaces in between."
let singleOption = (optionIdentifier .>>. ((restOfLine false) .>> (newline .>>. spaces))) |>> TabOption
let option = (many singleOption) .>> spaces


//***************** Chord charts

// Barre
let stringsBody = digit .>>. (pstr "-" >>. digit) <!> "stringsBody"
let strings = pstr "strings:" >>. (spaces >>. stringsBody) <!> "strings"
let fret =  pstr "fret:" >>. (spaces >>. digit) <!> "fret"

// fret is what fret the barre is on, strings tells the start and end string for the bar
let barreInside = ((spaces >>. fret) .>>. (spaces >>. strings)) .>> spaces <!> "barreInside"

// barre is inside []
let barre = pstr "barre:" >>. (spaces >>. (between (pstr "[") (pstr "]") barreInside)) |>> (fun (a,(b,c)) -> (int a - _0, int b - _0, int c - _0)) |>> Barre <!> "barre"

// Spots
let spot = digit .>>. (pstr "-" >>. digit) |>> (fun (a,b) -> (int a - _0, int b - _0)) |>> Spot <!> "spot"
let spotsBody = between spaces spaces spot <!> "spotsBody"
let spots = many spotsBody <!> "spots"

// empty barre - just preturn
let emptyBarre = preturn EmptyBarre

// full chart
let chartBody = (barre <|> emptyBarre) .>>. spots |>> Chart <!> "chartBody"

let chart : Parser<_> = (between (pstr "[") (pstr "]") (between spaces spaces chartBody)) .>> spaces <!> "chart"
let charts = many chart <!> "charts"

let tabExpr = (option .>>. charts) .>> spaces <!> "tabExpr"

let tabGrammar : Parser<_> = tabExpr .>> eof <!> "tabGrammar"
