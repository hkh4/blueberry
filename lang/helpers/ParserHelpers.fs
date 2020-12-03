module ParserHelpers

open System
open FParsec

// Generic types to avoid Value Restriction error
type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>


let pstr s = pstring s
// Helper parse that parses one word
let word : Parser<_> = many1Satisfy (fun c -> c <> ' ' && c <> '\n' && c <> '\r' && c <> '\t') <??> "Expecting a word"
// Single space
let regularSpace : Parser<_> = pchar ' ' <??> "Expecting a space"
// Multiple spaces
let multipleSpaces : Parser<_> = manySatisfy (fun c -> c = ' ') <??> "Expecting whitespace"
let multipleSpaces1 : Parser<_> = many1Satisfy (fun c -> c = ' ') <??> "Expecting whitespace"
// debugging
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
   fun stream ->
      //printfn "%A: Entering %s" stream.Position label
      let reply = p stream
      //printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
      reply
