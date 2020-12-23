module Music

open System
open TabTypes



(**)
let rec wordToString (str: string) (res: string) : string =

   // first turn all dashes into spaces
   let s = str.Replace("-", " ")

   // Keep looking for chord markers until there are no more left.
   match s.IndexOf("/")  with
   | -1 ->
      // if there are no more stubs, just return the rest of the word
      let last = " (" + s + ") "
      res + last + " ( ) "

   // first slash
   | firstSlash ->


      // find the ending slash
      let afterFirstSlash = s.[firstSlash+1..]
      let secondSlash = afterFirstSlash.IndexOf("/")

      match secondSlash with
      | -1 ->

         // if there is no second slash, then assume this slash is part of the lyrics and return the rest of it
         let ending = " (" + s + ") ( ) "
         res + ending

      | secondSlash ->

         // first, get the section up until the first slash
         let beforeFirstSlash = s.[0..firstSlash - 1]
         let beforeFirstSlashString = " (" + beforeFirstSlash + ") "

         // then, create the chord by adding a 1, and then the chord itself
         let betweenSlashes = s.[firstSlash + 1 .. firstSlash + secondSlash]
         let chordText = " 1 (" + betweenSlashes + ") "

         // finally, recurse on the rest of the string
         let restOfString = s.[firstSlash + secondSlash + 2..]

         wordToString restOfString (res + beforeFirstSlashString + chordText)



(* Evaluate a single line
1) line is the singleLine that includes the text for that line
*)
let evalLine (line: singleLine) : string option =

   match line with
   | SingleLine(s) ->

      let trimmed = s.Trim(' ')

      // if the entire line is empty, don't make such a big gap
      match trimmed with
      | "" -> Some(""" (\sm) """)
      | _ ->

         let listOfWords = trimmed.Split(" ") |> Array.toList

         let fullString = List.fold (fun acc c -> acc + (wordToString c "")) "" listOfWords

         let withNewLine = fullString + """ (\n) """

         Some(withNewLine)






(* Evaluate the lines of the lyrics
1) lines is the list of lines
2) x is the x coord
3) y is the y coord
4) text is the text code
RETURNS the text code or none
*)
let rec evalLines (lines: singleLine list) (x: float) (y: float) (text: string) : string option =

   match lines with
   | [] ->

      let fullText = " " + string x + " " + string y + " [" + text + "] line "
      Some(fullText)

   | head::tail ->

      match evalLine head with
      | Some(lineText) ->
         evalLines tail x y (text + lineText)

      | None -> None





(* Driver for evaluating the lyrics
1) music is the Music types which contains all the lines
2) x is the starting x coord
3) y is the starting y coord
RETURNS the text code or none
*)
let evalMusic (music: TabExpr) (x: float) (y: float) : string option =

   match music with
   | Music(lines) ->

      // if x is greater than 100, there was at least one chart, so skip to the next line
      let newY =
         if x > 100.0 then y - 50.0
         else y + 20.0

      match evalLines lines chartStartX newY "" with
      | Some(musicText) -> Some(musicText)
      | None -> None

   | _ ->
      printfn "Error! Given a type that is not Music in evalMusic."
      None































//
