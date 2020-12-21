module TabOptions

open System
open TabTypes

(* Parses a single option
1) ops is a single option
2) optionsR is the record that holds all the option info so far
RETURNS the updated optionsRecord or None if an error
*)
let parseOptions (opt: TabExpr) (optionsR: optionsRecord) : optionsRecord option =

   // Find the right case. If not valid, return none. Otherwise, update the optionsR and return
   match opt with

   // title
   | TabOption(key, value) when key = "title" ->
      let valueTrim = value.Trim(' ')
      let newOption = { optionsR with Title = valueTrim }
      Some(newOption)

   // composer
   | TabOption(key, value) when key = "composer" ->
      let valueTrim = value.Trim(' ')
      let newOption = { optionsR with Composer = valueTrim }
      Some(newOption)

   // capo
   | TabOption(key, value) when key = "capo" ->
      let valueTrim = value.Trim(' ')
      try
         let capo = int valueTrim
         let newOption = { optionsR with Capo = capo }
         Some(newOption)
      with
         | _ ->
            printfn "The capo option must be an integer"
            None

   // key
   | TabOption(key, value) when key = "key" ->
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

   | TabOption(key, value) when key = "tuning" ->
      let valueTrim = value.Trim(' ')
      let newOption = { optionsR with Tuning = valueTrim }
      Some(newOption)

   | _ ->
      printfn "Invalid option! Valid options are title, composer, capo, tuning, and key"
      None

(* Driver for parsing options
1) opts are all the options
2) optionsR is the record that holds all the info. Begins as the default
RETURNS the completed optionsR, or an error, as well as the options text
*)
let rec evalOption (opts: TabExpr List) (optionsR: optionsRecord) : (optionsRecord * String) option =
   match opts with
   | [] ->
      let optionsText = " (" + optionsR.Title + ") title " + "(" + optionsR.Composer + ") composer (capo " + string optionsR.Capo + ") capo (Tuning: " + optionsR.Tuning + ") tuning "
      Some(optionsR, optionsText)
   | head::tail ->
      match (parseOptions head optionsR) with
      | Some(newOptionsR) -> evalOption tail newOptionsR
      | None -> None





















//
