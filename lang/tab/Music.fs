module Music

open System
open TabTypes


let evalLine (line: singleLine) (x: float) (y: float) : (string * float * float) option =

   Some("", x, y)





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
      | Some(lineText, newX, newY) ->
         evalLines tail newX newY (text + lineText)

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

      match evalLines lines x y "" with
      | Some(musicText) -> Some(musicText)
      | None -> None

   | _ ->
      printfn "Error! Given a type that is not Music in evalMusic."
      None































//
