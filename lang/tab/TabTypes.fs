module TabTypes

open System
open FParsec

// Generic types to avoid Value Restriction error
type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>
