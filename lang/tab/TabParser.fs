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
let options = (many singleOption) .>> spaces


//***************** Chord charts

// Barre
let stringsBody = digit .>>. (pstr "-" >>. digit) <!> "stringsBody"
let strings = pstr "strings:" >>. (spaces >>. stringsBody) <!> "strings"
let fret =  pstr "fret:" >>. (spaces >>. digit) <!> "fret"

// fret is what fret the barre is on, strings tells the start and end string for the bar
let barreInside = ((spaces >>. fret) .>>. (spaces >>. strings)) .>> spaces <!> "barreInside"

// barre is inside []
let barre = pstr "barre:" >>. (spaces >>. (between (pstr "[") (pstr "]") barreInside)) |>> (fun (a,(b,c)) -> (int a - _0, int b - _0, int c - _0)) |>> Barre <!> "barre"

// empty barre - just preturn
let emptyBarre = preturn EmptyBarre |>> (fun a -> [a]) <!> "emptyBarre"

let barres = many1 (between spaces spaces barre)

let anyBarre = barres <|> emptyBarre <!> "anyBarre"


// Spots
let spotBeginning = digit .>> (pstr "-")

let spot = spotBeginning .>>.? digit |>> (fun (a,b) -> (int a - _0, int b - _0)) |>> Spot <!> "spot"

let xSpot = spotBeginning .>> (pstr "X") |>> (fun a -> int a - _0) |>> XSpot <!> "xspot"

let spotsBody = between spaces spaces (spot <|> xSpot) <!> "spotsBody"
let spots = many spotsBody <!> "spots"

// title
let title = (pstr "title:") >>. (restOfLine true)

// full chart
let chartBody = tuple3 title anyBarre spots |>> Chart <!> "chartBody"

let chart : Parser<_> = (between (pstr "[") (pstr "]") (between spaces spaces chartBody)) .>> spaces <!> "chart"
let charts = many chart <!> "charts"

//*** Words

let line = restOfLine false |>> SingleLine

let lines = sepBy line newline |>> Music

let tabExpr = (tuple3 options charts lines) .>> spaces <!> "tabExpr"

let tabGrammar : Parser<_> = tabExpr .>> eof <!> "tabGrammar"
