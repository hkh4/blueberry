module Music

open System
open TabTypes



(**)
let rec lineToString (s: string) (res: string) : string =

   // Keep looking for chord markers until there are no more left.
   match s.IndexOf("/")  with
   | -1 ->
      // if there are no more stubs, just return the rest of the word
      let last = " (" + s + ") "
      res + last + " ( ) "

   | firstSlash ->
      // find the ending slash
      let afterFirstSlash = s.[x+1..]
      let sec = afterFirstSlash.IndexOf("/")

      match secondSlash with
      | -1 ->
      | secondSlash ->



(* Evaluate a single line
1) line is the singleLine that includes the text for that line
*)
let evalLine (line: singleLine) : string option =

   match line with
   | SingleLine(s) ->

      let trimmed = s.Trim(' ')

      let listOfWords = trimmed.Split(" ") |> Array.toList

      let fullString = List.fold (fun acc c -> acc + (lineToString c "")) "" listOfWords

      Some(" " + string x + " " + string y + " [ " + fullString + " ] ", x, y)





(* Evaluate the lines of the lyrics
1) lines is the list of lines
2) x is the x coord
3) y is the y coord
4) text is the text code
RETURNS the text code or none
*)
let rec evalLines (lines: singleLine list) (x: float) (y: float) (text: string) : string option =

   match lines with
   | [] -> Some(text)
   | head::tail ->

      match evalLine head x y with
      | Some(lineText) ->
         evalLines tail (text + lineText)

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
         else y

      match evalLines lines chartStartX newY "" with
      | Some(musicText) -> Some(musicText)
      | None -> None

   | _ ->
      printfn "Error! Given a type that is not Music in evalMusic."
      None































//
