module Options

open System
open Types

// ******************* EVALUATE OPTIONS **********************

(* parse and assign the options
1) a is an Expr which should be a ScoreOption
2) optionR is the options record to be modified
RETURNS an option, the bool is really just a placeholder
*)
let parseOptions (a : Expr) (optionsR : optionsRecord) : optionsRecord option =
   match a with

   // If time
   | ScoreOption(key: string, value: string) when key = "time" ->
      let valueTrim = value.Trim(' ')
      let timeArray = valueTrim.Split('-')
      // make sure it's length 2
      match (timeArray.Length) with
      | 2 ->
         // try splitting up the array, turning to int, but catch exception
         try
            let timeTuple = ((int timeArray.[0]),(int timeArray.[1]))
            // make sure the top number is valid
            match (int timeArray.[0]) with
            | num when (num >= 1 && num <= 32) || num = 64 ->
               let newOption = { optionsR with Time = timeTuple }
               // make sure the bottom number is valid, update the defaultRhythm and optionsRecord
               match (int timeArray.[1]) with
               | 1 ->
                  defaultRhythm <- R(X1,0)
                  Some(newOption)
               | 2 ->
                  defaultRhythm <- R(X2,0)
                  Some(newOption)
               | 4 ->
                  defaultRhythm <- R(X4,0)
                  Some(newOption)
               | 8 ->
                  defaultRhythm <- R(X8,0)
                  Some(newOption)
               | 16 ->
                  defaultRhythm <- R(X16,0)
                  Some(newOption)
               | 32 ->
                  defaultRhythm <- R(X32,0)
                  Some(newOption)
               | 64 ->
                  defaultRhythm <- R(X64,0)
                  Some(newOption)
               | _ ->
                  printfn "The second number of the time signature can be 1, 2, 4, 8, 16, 32, or 64"
                  None
            | _ ->
               printfn "The first number of the time signature can be 1-32 or 64"
               None
         // catch
         with
            | _ ->
               printfn "The time option should be of the form (int)-(int)"
               None
      | _ ->
         printfn "The time option should be of the form (int)-(int)"
         None

   // If key
   | ScoreOption(key: string, value: string) when key = "key" ->
      let valueTrim = value.Trim(' ')
      match valueTrim with
      | "c" | "cm" | "c#" | "c#m" | "cb" | "d" | "dm" | "db" | "d#m" | "e" | "em" | "eb" | "ebm" | "f" | "fm" | "f#m" ->
         let newOption = { optionsR with Key = valueTrim }
         Some(newOption)
      | "f#" | "g" | "gm" | "g#m" | "gb" | "a" | "am" | "a#m" | "ab" | "abm" | "b" | "bm" | "bb" | "bbm" ->
         let newOption = { optionsR with Key = valueTrim }
         Some(newOption)
      | _ ->
         printfn "Invalid key. Valid options are: c cm c# c#m cb d dm db d#m e em eb ebm f fm f#m f# g gm g#m gb a am a#m ab abm b bm bb bbm"
         None

   // If capo
   | ScoreOption(key: string, value: string) when key = "capo" ->
      let valueTrim = value.Trim(' ')
      try
         let capo = int valueTrim
         let newOption = { optionsR with Capo = capo }
         Some(newOption)
      with
         | _ ->
            printfn "The capo option must be an integer"
            None

   // Title
   | ScoreOption(key: string, value: string) when key = "title" ->
      let valueTrim = value.Trim(' ')
      let newOption = { optionsR with Title = valueTrim }
      Some(newOption)

   // Composer
   | ScoreOption(key: string, value: string) when key = "composer" ->
      let valueTrim = value.Trim(' ')
      let newOption = { optionsR with Composer = valueTrim }
      Some(newOption)

   | ScoreOption(key, value) when key = "tuning" ->
      let valueTrim = value.Trim(' ')

      // split it up into the 6 strings
      let stringsKeys = valueTrim.Split "-"

      match stringsKeys.Length with
      | 6 ->
        let arrayOfNumbers =
          Array.collect (fun elem ->
            match elem with
              | "e" | "fb" -> [|0|]
              | "f" | "e#" -> [|11|]
              | "f#" | "gb" -> [|10|]
              | "g" -> [|9|]
              | "g#" | "ab" -> [|8|]
              | "a" -> [|7|]
              | "a#" | "bb" -> [|6|]
              | "b" | "cb" -> [|5|]
              | "c" | "b#" -> [|4|]
              | "c#" | "db" -> [|3|]
              | "d" -> [|2|]
              | "d#" | "eb" -> [|1|]
              | _ ->
                 printfn "Error in parseOptions. A note with Pitch type NoPitch should never enter this function"
                 [|-1|]) stringsKeys

        let listOfNumbers = Array.toList arrayOfNumbers

        let newOption = { optionsR with TuningNumbers = listOfNumbers; Tuning = valueTrim }
        Some(newOption)
      | _ ->
        printfn "Tuning should have exactly 6 strings"
        None


   // Notes
   | _ ->
      printfn "Invalid option! Valid options are key, title, composer, capo, arranger, tuning, and time"
      None




(* driver to update the options record
1) o is the list of ScoreOption
2) optionsR is the options record to be updated
RETURNS the updated options record
*)
let rec evalOption (o: Expr List) (optionsR: optionsRecord) : optionsRecord option =
   match o with
   | [] ->
      Some(optionsR)
   | head::tail ->
      match (parseOptions head optionsR) with
         | Some(newOption) -> evalOption tail newOption
         | None -> None
