// Learn more about F# at http://fsharp.org

open System
open System.IO
open FParsec
open ScoreParser
open ScoreInterpreter
open TabParser
open TabInterpreter

[<EntryPoint>]
let main argv =
   //reads in from a file
   if (Array.length argv <> 3) then
      printfn "Correct usage: dotnet run <type> <file> <outFile>"
      1
   else
      let programType = argv.[0]
      let fileName = argv.[1]
      let outFile = argv.[2]
      let fileText = System.IO.File.ReadAllText fileName

      match programType with
      | "score" ->
         match run grammar fileText with
         | Success(result, _, _) ->
            let (optionsList, measuresList) = result
            // printfn "%A" result
            match eval optionsList measuresList outFile with
            | Some(text,pages) -> 0
            | None -> 1
         | Failure(errorMsg, _, _) ->
            printfn "Failure: %s" errorMsg
            1

      | "tab" ->
         match run tabGrammar fileText with
         | Success(result, _, _) ->
            match tabEval result outFile with
            | 0 ->
               //printfn "%A" message
               0
            | _ -> 1
         | Failure(errorMsg, _, _) ->
            printfn "Failure: %s" errorMsg
            1

      | _ ->
         printfn "Correct usage: dotnet run <type> <file> <outFile>. The allowed types are 'score' and 'tab'."
         1
